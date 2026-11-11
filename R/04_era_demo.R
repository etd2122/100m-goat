# =============================================================================
# 04_era_demo.R
# Purpose: Apply the same leave-one-out z-score to baseball: every ERA-title
#          qualifier is scored against the other qualifiers in his league
#          that season, then compared with ERA+, the stat baseball already
#          uses to adjust ERA for context.
# Input:   the Lahman R package (Pitching, Teams, People tables)
#          install once with renv::install("Lahman"); renv::snapshot()
# Output:  data/processed/era_demo.csv  one row per qualifying pitcher-season
# =============================================================================

library(Lahman)
library(dplyr)
library(readr)

# ---- Pitcher-seasons, one row per pitcher per league per year --------------------
# A pitcher traded within a league has one row per team ("stint"); add them up.
# A pitcher traded across leagues is scored separately in each league, since
# each league is its own field.
pitching <- Pitching |>
  filter(yearID >= 1901, lgID %in% c("AL", "NL")) |>
  # Attach each team's pitching park factor (PPF) for ERA+
  left_join(Teams |> select(yearID, teamID, PPF), by = c("yearID", "teamID")) |>
  group_by(playerID, yearID, lgID) |>
  summarise(
    # PPF goes first: summarise() reuses new columns, so if IPouts were
    # summed first, the weights below would be the total, not per stint.
    # A pitcher with two teams gets an innings-weighted park factor.
    PPF    = weighted.mean(PPF, w = pmax(IPouts, 1)),
    IPouts = sum(IPouts, na.rm = TRUE),
    ER     = sum(ER,     na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(IP = IPouts / 3)

# ---- League context ----------------------------------------------------------------
# League ERA uses EVERY pitcher (not just qualifiers): that is the run
# environment ERA+ compares against.
# Games: the ERA title needs 1 inning per team game. Seasons were shorter
# before 1961 (AL) / 1962 (NL) and in strike years, so take the league's
# longest team schedule that year rather than assuming 162.
league <- pitching |>
  group_by(yearID, lgID) |>
  summarise(lgERA = 9 * sum(ER) / sum(IP), .groups = "drop") |>
  left_join(Teams |> group_by(yearID, lgID) |> summarise(games = max(G), .groups = "drop"),
            by = c("yearID", "lgID"))

# ---- Qualifiers and scores -------------------------------------------------------------
loo_sd <- function(x) vapply(seq_along(x), \(i) sd(x[-i]), numeric(1))

era_demo <- pitching |>
  left_join(league, by = c("yearID", "lgID")) |>
  filter(IP >= games) |>                       # ERA-title qualifiers only
  mutate(
    ERA      = 9 * ER / IP,
    # ERA+ (approximate): league ERA over pitcher ERA, scaled by park.
    # PPF > 100 = hitter-friendly park, so a given ERA there is worth more.
    ERA_plus = 100 * (lgERA * PPF / 100) / ERA
  ) |>
  group_by(yearID, lgID) |>
  mutate(
    n_field     = n(),
    mean_others = (sum(ERA) - ERA) / (n_field - 1),
    sd_others   = loo_sd(ERA),
    # Lower ERA is better, so others' mean minus own ERA: positive = better
    z           = (mean_others - ERA) / sd_others
  ) |>
  ungroup() |>
  left_join(People |> select(playerID, nameFirst, nameLast), by = "playerID") |>
  transmute(pitcher = paste(nameFirst, nameLast), yearID, lgID, IP = round(IP, 1),
            ERA, ERA_plus, lgERA, PPF, n_field, z) |>
  arrange(desc(z))

# ---- Validation ------------------------------------------------------------------------
# 1. Every league-season needs enough qualifiers for an SD (expect roughly 15-50)
era_demo |> count(yearID, lgID) |> arrange(n) |> head(5)
stopifnot(all(era_demo$n_field >= 3), all(is.finite(era_demo$z)))

# 2. Spot-check against Baseball-Reference: Pedro Martinez 2000 should show
#    ERA 1.74 and ERA+ near 291 (B-Ref's own park factors differ slightly,
#    so expect close, not exact). Bob Gibson 1968: ERA 1.12, ERA+ near 258.
era_demo |> filter(pitcher %in% c("Pedro Martinez", "Bob Gibson"),
                   yearID %in% c(2000, 1968))

# ---- Look at the results ------------------------------------------------------------------
# How closely do the two context adjustments agree? Spearman = rank agreement.
cor(era_demo$z, era_demo$ERA_plus, method = "spearman")
era_demo |> slice_head(n = 10)
era_demo |> arrange(desc(ERA_plus)) |> slice_head(n = 10)

# ---- Save -----------------------------------------------------------------------------------
write_csv(era_demo, "data/processed/era_demo.csv")
