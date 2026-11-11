# =============================================================================
# 03_robustness.R
# Purpose: Re-rank careers under reasonable alternative choices and record
#          where each of the baseline top 10 lands. If a runner's rank only
#          holds under one choice, the write-up has to say so.
# Input:   data/processed/finals.csv   (from 01_clean.R)
#          data/processed/rankings.csv (from 02_score.R, the baseline)
# Output:  data/processed/robustness.csv  one row per runner x specification
# =============================================================================

library(readr)
library(dplyr)
library(tidyr)

finals   <- read_csv("data/processed/finals.csv",   show_col_types = FALSE)
baseline <- read_csv("data/processed/rankings.csv", show_col_types = FALSE) |>
  filter(ranking == "career")

# Same field rule as 02_score.R: normal finishes + annulled runs count;
# injured, DNF and DQ do not.
field <- finals |> filter(status %in% c("OK", "annulled"), !is.na(time))

# Leave-one-out helpers. Each returns one value per runner, computed on the
# OTHER runners in the same final.
loo <- function(x, f) vapply(seq_along(x), \(i) f(x[-i]), numeric(1))

# ---- Score a final under a given scoring rule ---------------------------------
# rule = "z"      baseline: (mean of others - time) / SD of others
#        "margin" raw margin in seconds: mean of others - time (no scaling)
#        "robust" (median of others - time) / MAD of others. One slow
#                 finisher moves the median and MAD far less than the mean
#                 and SD, which is the weakness flagged in Limitations.
score_field <- function(d, rule) {
  d |>
    group_by(year, competition) |>
    mutate(score = case_when(
      rule == "z"      ~ (loo(time, mean) - time) / loo(time, sd),
      rule == "margin" ~  loo(time, mean) - time,
      rule == "robust" ~ (loo(time, median) - time) / loo(time, mad)
    )) |>
    ungroup()
}

# ---- Career ranking from a set of scores --------------------------------------
career_rank <- function(s, min_finals = 2) {
  s |>
    group_by(Competitor) |>
    summarise(n_finals = n(), mean_score = mean(score), .groups = "drop") |>
    filter(n_finals >= min_finals) |>
    arrange(desc(mean_score)) |>
    mutate(rank = row_number())
}

z_scores <- score_field(field, "z")

# ---- Empirical-Bayes shrinkage --------------------------------------------------
# A 2-final average is noisier than an 8-final one. Shrinkage pulls each
# career mean toward the overall mean by an amount that depends on how many
# finals it rests on (the same idea as regressing a hitter's 50-PA average
# toward league average).
#   sigma2 = within-runner variance of race scores (pooled)
#   tau2   = true between-runner variance = var(career means) - avg noise
#   B      = tau2 / (tau2 + sigma2 / n)      weight on the runner's own mean
shrunk_rank <- function(s, min_finals = 2) {
  per <- s |> group_by(Competitor) |>
    summarise(n = n(), m = mean(score), v = var(score), .groups = "drop")
  sigma2 <- with(filter(per, n > 1), sum(v * (n - 1)) / sum(n - 1))
  grand  <- mean(s$score)
  tau2   <- max(var(per$m) - mean(sigma2 / per$n), 0)
  per |>
    mutate(B = tau2 / (tau2 + sigma2 / n),
           mean_score = grand + B * (m - grand)) |>
    filter(n >= min_finals) |>
    arrange(desc(mean_score)) |>
    transmute(Competitor, n_finals = n, mean_score, rank = row_number())
}

# ---- The specifications ----------------------------------------------------------
specs <- bind_rows(
  career_rank(z_scores)                                  |> mutate(spec = "Baseline"),
  career_rank(z_scores, min_finals = 3)                  |> mutate(spec = "Minimum 3 finals"),
  career_rank(score_field(field, "margin"))              |> mutate(spec = "Raw margin (seconds)"),
  career_rank(score_field(field, "robust"))              |> mutate(spec = "Median / MAD"),
  # Annulled runs removed from the race entirely, so everyone else is
  # re-scored against a field without them.
  career_rank(score_field(filter(field, status != "annulled"), "z")) |>
    mutate(spec = "Annulled runs removed"),
  shrunk_rank(z_scores)                                  |> mutate(spec = "Shrunk toward mean")
)

# ---- Validation ------------------------------------------------------------------
# 1. Baseline here must match 02_score.R exactly, or the two scripts disagree.
check <- specs |> filter(spec == "Baseline") |>
  inner_join(baseline, by = "Competitor", suffix = c("", "_02"))
stopifnot(nrow(check) == nrow(baseline),
          all(check$rank == check$rank_02),
          all(abs(check$mean_score - check$mean_score_02) < 1e-9))

# 2. MAD can be 0 if the other finishers' times are tied; that would divide by 0.
stopifnot(all(is.finite(score_field(field, "robust")$score)))

# ---- Look at the results -----------------------------------------------------------
top10 <- baseline |> slice_head(n = 10) |> pull(Competitor)
specs |>
  filter(Competitor %in% top10) |>
  select(Competitor, spec, rank) |>
  pivot_wider(names_from = spec, values_from = rank) |>
  arrange(Baseline) |>
  print(width = Inf)

# ---- Save ---------------------------------------------------------------------------
write_csv(specs, "data/processed/robustness.csv")
