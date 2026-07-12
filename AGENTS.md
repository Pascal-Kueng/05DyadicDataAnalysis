# Repository Guidelines

This repository contains an R Markdown slide deck and supporting R functions for dyadic data analysis. Use the sections below to keep contributions consistent and reproducible.

## Project Structure & Module Organization
- `DyadicDataAnalysis.Rmd` is the primary slide source; `dyadic-data-analysis-tutorial.Rmd` is the PDF variant.
- `00_R_Functions/` holds reusable R helpers (e.g., `PrepareData.R`, `PrettyTables.R`, `ReportModels.R`).
- `Embed/` stores bibliography/assets used by the slides.
- Rendered artifacts live in `DyadicDataAnalysis.html`, `dyadic-data-analysis-tutorial.pdf`, and their `*_files/` directories.
- `renv/` and `renv.lock` pin package versions; `brms_cache/` keeps model cache files used by the slides.

## Build, Test, and Development Commands
- `renv::restore()` installs the pinned R package set from `renv.lock`.
- `quarto render DyadicDataAnalysis.Rmd` rebuilds the HTML slides.
- `quarto render dyadic-data-analysis-tutorial.Rmd` rebuilds the PDF output.
- Optional (for Bayesian models): `cmdstanr::install_cmdstan()` after installing `cmdstanr`.

## Coding Style & Naming Conventions
- Follow tidyverse-style R conventions: 2-space indentation, `<-` for assignment, and `snake_case` for objects and functions.
- Keep roxygen2 headers on exported helpers in `00_R_Functions/`.
- Prefer explicit names (`is_` prefix for dummy variables) and keep data preparation logic in the helper scripts, not embedded in long chunks.

## Testing Guidelines
- There is no automated test suite; validate changes by rendering the slides and checking key figures/tables.
- If you add tests, place them under `tests/testthat/` with `test-*.R` names and document how to run them.

## Commit & Pull Request Guidelines
- Recent history uses short, sentence-case messages (e.g., "added pdf version"); keep commits concise and descriptive.
- PRs should summarize changes, list any regenerated outputs (`DyadicDataAnalysis.html`, `dyadic-data-analysis-tutorial.pdf`), and call out cache changes if they affect results.
