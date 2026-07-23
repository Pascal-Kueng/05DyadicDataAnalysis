# Tutorial: Distinguishable and Exchangeable Dyads: Bayesian Multilevel Modelling

*Cross-sectional and intensive longitudinal APIM and DIM*

[![DOI](https://zenodo.org/badge/1079959998.svg)](https://doi.org/10.5281/zenodo.17400655)

## Access the tutorial

Choose the interactive slide deck, the archival PDF, or one of the concise model guides:

- **[Interactive HTML tutorial](https://pascal-kueng.github.io/05DyadicDataAnalysis/DyadicDataAnalysis.html)**
- **[Archival PDF tutorial](https://pascal-kueng.github.io/05DyadicDataAnalysis/dyadic-data-analysis-tutorial.pdf)**
- **[Distinguishable dyads quick guide](https://pascal-kueng.github.io/05DyadicDataAnalysis/distinguishable-dyads-quick-guide.pdf)**
- **[Exchangeable dyads quick guide](https://pascal-kueng.github.io/05DyadicDataAnalysis/exchangeable-dyads-quick-guide.pdf)**
- **[Tutorial overview and citation](https://pascal-kueng.github.io/05DyadicDataAnalysis/)**
- **[`dyadMLM` data-preparation package](https://pascal-kueng.github.io/dyadMLM/)**
- **[Download the latest release](https://github.com/Pascal-Kueng/05DyadicDataAnalysis/releases/latest)**

---


## Reproduce everything locally

If you want to run all analyses or rebuild the slides locally:

1. [Download the latest release](https://github.com/Pascal-Kueng/05DyadicDataAnalysis/releases/latest) or clone the repository.
2. **Open** the `00DyadicDataAnalysis.Rproj`.
3. **Open** the presentation `DyadicDataAnalysis.Rmd`.
4. Restore the project packages in the R console:

   ```r
   renv::restore()
   ```

5. To run the Bayesian models, install CmdStan:

   ```r
   cmdstanr::check_cmdstan_toolchain(fix = TRUE)
   cmdstanr::install_cmdstan()
   ```

6. Run the code interactively, or rebuild either output from the terminal:

   ```sh
   quarto render DyadicDataAnalysis.Rmd --to revealjs --output DyadicDataAnalysis.html
   quarto render DyadicDataAnalysis.Rmd --to pdf --output dyadic-data-analysis-tutorial.pdf
   ```

Both outputs are generated from the same format-aware source. Interactive
elements are included only in HTML; the PDF uses static replacements and more
compact table formatting.

---

Tutorial content is licensed under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/); code is licensed under the [MIT License](LICENSE-CODE).
