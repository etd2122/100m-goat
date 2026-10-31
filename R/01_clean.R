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

