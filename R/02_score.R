# =============================================================================
# 02_score.R
# Purpose: Score each finalist's dominance over the rest of their final,
#          then rank runners by best single race and by career average.
# Input:   data/processed/finals.csv   (from 01_clean.R)
# Output:  data/processed/scores.csv   one row per scored runner per final
#          data/processed/rankings.csv one row per runner
# =============================================================================

library(readr)
library(dplyr)

finals <- read_csv("data/processed/finals.csv", show_col_types = FALSE)

# ---- Who counts in the field statistics ---------------------------------------
# Included: normal finishes (OK) and annulled doping runs (races "as run").
# Excluded: injured finishers (time reflects injury, not ability) and
#           DNF/DQ (no time). They stay in finals.csv but are not scored.
scored_field <- finals |>
  filter(status %in% c("OK", "annulled"), !is.na(time))

# ---- Leave-one-out dominance score --------------------------------------------
# For each runner, compare their time to the OTHER runners in the same final:
#   score = (mean of others - own time) / SD of others
# Positive = faster than the field; +3 = three standard deviations clear.
# Leaving the runner out stops a dominant winner from inflating their own
# field's average and spread.
loo_sd <- function(x) vapply(seq_along(x), \(i) sd(x[-i]), numeric(1))

scores <- scored_field |>
  group_by(year, competition) |>
  mutate(
    n_field     = n(),
    mean_others = (sum(time) - time) / (n_field - 1),
    sd_others   = loo_sd(time),
    score       = (mean_others - time) / sd_others
  ) |>
  ungroup() |>
  arrange(desc(score))

# ---- Rankings -----------------------------------------------------------------
# Best single race: each runner's highest score.
best_race <- scores |>
  group_by(Competitor) |>
  slice_max(score, n = 1, with_ties = FALSE) |>
  ungroup() |>
  arrange(desc(score)) |>
  transmute(rank = row_number(), Competitor, Nat, year, competition,
            time, score, status)

# Career: average score across all championship finals (minimum 2 finals).
career <- scores |>
  group_by(Competitor) |>
  summarise(
    Nat          = first(Nat),
    n_finals     = n(),
    mean_score   = mean(score),
    best_score   = max(score),
    titles       = sum(place == 1, na.rm = TRUE),
    has_annulled = any(status == "annulled"),
    .groups = "drop"
  ) |>
  filter(n_finals >= 2) |>
  arrange(desc(mean_score)) |>
  mutate(rank = row_number(), .before = 1)

# ---- Validation -----------------------------------------------------------------
# 1. Field size used for scoring in each final (expect 6-9; small = check why)
scores |> distinct(year, competition, n_field) |> arrange(n_field) |> head(5)

# 2. Same person under two spellings? Lists surnames with >1 distinct full name.
#    Any hits need a name fix in supplement.csv (e.g. "Ray" vs "Raymond").
scores |>
  mutate(surname = sub(".* ", "", Competitor)) |>
  group_by(surname) |>
  filter(n_distinct(Competitor) > 1) |>
  distinct(surname, Competitor) |>
  arrange(surname)

# 3. Hand-check one score: Bolt, Berlin 2009
scores |> filter(year == 2009, place == 1) |>
  select(Competitor, time, mean_others, sd_others, n_field, score)

# ---- Look at the results ----------------------------------------------------------
print(best_race, n = 15)
print(career, n = 15)

# ---- Save ---------------------------------------------------------------------------
write_csv(scores,  "data/processed/scores.csv")
write_csv(bind_rows(
  best_race |> mutate(ranking = "best_race"),
  career    |> mutate(ranking = "career")
), "data/processed/rankings.csv")
