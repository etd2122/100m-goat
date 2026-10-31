# Who Is Defining Greatness? Measuring Dominance in Championship 100m Finals

**Read the paper:** https://etd2122.github.io/100m-goat/

Extending my final presentation for CLCV OC3330: Ancient and Modern Olympics (Columbia University, Summer 2026, Athens), this project scores every finalist in 35 Olympic and World Championship men's 100m finals (1968–2025) by how far they beat the other runners in their own race. The within-race comparison removes race-level confounders such as wind, altitude, track and era by the same logic as fixed effects. Usain Bolt ranks first on both career and single-race dominance.

## Repository structure
- `R/01_clean.R`: builds and validates the finals dataset (toplist data plus a sourced, hand-entered supplement)
- `R/02_score.R`: computes leave-one-out dominance scores and rankings
- `index.qmd`: the paper (Quarto), rendered to `docs/` for GitHub Pages
- `data/raw/`: men's 100m subset, championship lookup, supplement with a source URL per row
- `data/processed/`: cleaned finals, scores and rankings

## Reproduce
1. Clone the repo and open `100m-goat.Rproj` in RStudio
2. Run `renv::restore()` to install the exact package versions
3. Run `R/01_clean.R`, then `R/02_score.R`
4. Run `quarto render` in the Terminal

The full World Athletics source file (50 MB) is not included; `01_clean.R` runs from the committed men's 100m subset. The source is the [world-athletics-database](https://github.com/thomascamminady/world-athletics-database).
