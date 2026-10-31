# =============================================================================
# 01_clean.R
# Purpose: Turn raw World Athletics toplist data into a clean table of
#          men's 100m Olympic and World Championship finals.
# Input:   data/raw/athletics_full.csv  (50 MB, kept off GitHub, see README)
# Output:  data/raw/athletics_100m.csv  (men's 100m only, committed)
# =============================================================================

library(readr)   # reading and writing CSV files
library(dplyr)   # filtering and transforming data

# ---- Part 1: Cut the full dataset down to men's 100m ------------------------
# The full scrape covers every event and is too big for GitHub (50 MB).
# We keep only the rows this project needs and save them as a small file
# that IS committed, so anyone who clones the repo can rerun the analysis.
# If the full file isn't present (e.g. on someone else's computer), we
# skip this step and read the small file directly.

full_path  <- "data/raw/athletics_full.csv"
small_path <- "data/raw/athletics_100m.csv"

if (file.exists(full_path)) {

  athletics_full <- read_delim(
    full_path,
    delim = ";",   # the source file uses semicolons, not commas
    # Read every column as text. If readr guesses column types itself, it
    # can silently turn unexpected values into NA. We convert types
    # deliberately in a later step, where we can check what went wrong.
    col_types = cols(.default = col_character())
  )

  men_100m <- athletics_full |>
    filter(Event == "100 Metres",
           Sex   == "male")

  write_csv(men_100m, small_path)
}

# From here on, the script only uses the small file.
men_100m <- read_csv(small_path,
                     col_types = cols(.default = col_character()))

# ---- Sanity checks -----------------------------------------------------------
# Expect roughly 24,875 rows (the count from the first version of this project).
nrow(men_100m)
glimpse(men_100m)


# ---- Part 2: Keep only championship finals ----------------------------------
# Each row is one performance. The Pos column encodes place AND round:
#   "3"    = 3rd in a final
#   "1sf2" = 1st in semifinal 2
#   "4h1"  = 4th in heat 1
# Semifinals are often run earlier on the same day as the final (e.g.
# Berlin 2009), so the date alone can't identify the final. We need both:
# a championship final date AND a Pos that is only a number.

# Hand-built lookup of the 35 Olympic / World Championship 100m finals.
# Read as text so final_date matches the text Date column in men_100m.
championships <- read_csv("data/raw/championships.csv",
                          col_types = cols(.default = col_character()))

candidates <- men_100m |>
  filter(grepl("^[0-9]+$", Pos)) |>           # bare number = a final
  inner_join(championships,                    # keep only championship dates
             by = c("Date" = "final_date")) |>
  # Other meets can share a championship date: e.g. a meet in Hendon,
  # London on the day of the 2007 Osaka final. On each date, keep only
  # the venue with the most finishers, which is the championship stadium.
  add_count(Date, Venue, name = "n_at_venue") |>
  group_by(Date) |>
  mutate(main_venue = n_at_venue == max(n_at_venue)) |>
  ungroup()

# Show what the venue rule threw out, so nothing disappears silently.
# Expect: Leevan Yearwood, Hendon (GBR), 2007.
candidates |>
  filter(!main_venue) |>
  select(Date, Venue, Competitor, Mark)

finals <- candidates |>
  filter(main_venue) |>
  mutate(year  = as.integer(year),
         place = as.integer(Pos),
         time  = as.numeric(Mark)) |>
  select(year, competition, city, Date, Venue, place,
         Competitor, Nat, time, Wind, official_winner, note) |>
  arrange(year, place)


# ---- Validation ---------------------------------------------------------------
# We match on date (plus the main-venue rule), because venue names in the data
# are inconsistent. These checks catch anything that slips through.

# 1. Did any time fail to convert to a number?
sum(is.na(finals$time))   # expect 0

# 2. One summary row per final we found
final_check <- finals |>
  group_by(year, competition, city) |>
  summarise(
    n_runners       = n(),                       # 8 normally; 9 in a 9-lane final (Beijing 2015)
    n_venues        = n_distinct(Venue),         # should be 1
    has_1st_place   = any(place == 1),           # should be TRUE
    dup_places      = any(duplicated(place)),    # TRUE only for a dead heat (Beijing 2015 bronze)
    winner_in_data  = Competitor[which.min(place)],
    official_winner = first(official_winner),
    .groups = "drop"
  )
print(final_check, n = 40, width = Inf)

# 3. Championships that matched NO rows at all
#    (expected: Budapest 2023, Paris 2024, Tokyo 2025, since the data ends July 2023)
championships |>
  anti_join(distinct(finals, Date), by = c("final_date" = "Date")) |>
  select(year, competition, city, final_date)

# 4. Any final failing a check? Expect 0 rows.
final_check |>
  filter(n_venues > 1 | n_runners > 9 | !has_1st_place)

# ---- Part 3: Add the hand-entered supplement --------------------------------
# The toplist only has marks of 10.30 or faster, ends in July 2023, and follows
# official results (doping-annulled runs removed). The supplement fills those
# gaps from official results pages, one sourced row per missing runner.
#   status = OK        normal finish
#            annulled  ran in the race, result later annulled for doping
#            injured   finished, but pulled up hurt (time not representative)
#            DNF / DQ  did not finish / disqualified (no time)

supplement <- read_csv("data/raw/supplement.csv",
                       col_types = cols(.default = col_character()))

finals_full <- bind_rows(
  # Rows from the toplist: all are normal finishes
  finals |>
    rename(final_note = note) |>
    mutate(status = "OK",
           source = "World Athletics all-time toplist (thomascamminady scrape)"),
  # Rows from the supplement: attach championship details by year
  supplement |>
    rename(runner_note = note) |>
    left_join(championships |>
                select(year, competition, city, final_date,
                       official_winner, final_note = note),
              by = "year") |>
    transmute(year = as.integer(year), competition, city,
              Date = final_date, place = as.integer(place),
              Competitor, Nat, time = as.numeric(time), Wind,
              official_winner, final_note, status, source, runner_note)
) |>
  arrange(year, is.na(place), place)

# ---- Validation: is every field complete? ------------------------------------
# 5. Entrants per final, counting annulled/DNF/DQ runners.
#    Expect 8 everywhere except Beijing 2015 (9 lanes).
field_check <- finals_full |>
  group_by(year, competition, city) |>
  summarise(n_entrants = n(),
            n_with_time = sum(!is.na(time)),
            .groups = "drop")
field_check |> filter(n_entrants != 8)   # expect only 2015 (9)
nrow(field_check)                          # expect 35 finals

# 6. Any runner entered twice in the same final? Expect 0 rows.
finals_full |> count(year, Competitor) |> filter(n > 1)

# ---- Save ---------------------------------------------------------------------
write_csv(finals_full, "data/processed/finals.csv")

s <- readr::read_lines("data/raw/supplement.csv")
s <- sub("Nesta CARTER,JAM,10.95,-1.4,OK,(.*),unusually slow; reason not given in source",
         "Nesta CARTER,JAM,10.95,-1.4,injured,\\1,reclassified injured: 0.68s behind 6th place; time treated as non-representative",
         s)
readr::write_lines(s, "data/raw/supplement.csv")
table(readr::read_csv("data/raw/supplement.csv", show_col_types = FALSE)$status)  # injured should now be 4
