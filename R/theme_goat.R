# =============================================================================
# theme_goat.R
# Purpose: One chart style for every figure on the site, so the charts and
#          the page (custom.scss) read as one design. Sourced by index.qmd.
#          Colours here must match the SCSS variables of the same name.
# =============================================================================

library(ggplot2)

# ---- Palette ---------------------------------------------------------------------
# ink    text and the main data colour (same as the page text)
# track  "this runner ranks higher once era is removed": the blue of a
#        modern track surface (Berlin 2009, Tokyo 2021)
# amber  the scoreboard accent; used sparingly, only to highlight
# grey   context and doping-annulled results, so they stay visible but quiet
# rule   gridlines
goat_cols <- c(
  ink   = "#14213D",
  track = "#24527A",
  amber = "#C98A0B",   # darker than the page's #E8A317 so it passes contrast on white
  grey  = "#9AA0A6",
  rule  = "#E3E6EA"
)

# ---- Theme -----------------------------------------------------------------------
# Built on theme_minimal: no chart border, horizontal gridlines only, titles
# left-aligned (the eye starts at the left, like the text column).
theme_goat <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      text               = element_text(colour = goat_cols[["ink"]]),
      axis.text          = element_text(colour = "#4A5363"),
      axis.title         = element_text(colour = "#4A5363", size = rel(0.9)),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = goat_cols[["rule"]], linewidth = 0.4),
      legend.position    = "top",
      legend.justification = "left",
      legend.title       = element_blank(),
      plot.title.position = "plot",
      plot.margin        = margin(8, 12, 8, 8)
    )
}
