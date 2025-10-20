# Distinguishable and Exchangeable Dyads: Multilevel Modelling
Cross-Sectional and Intensive Longitudinal APIM and DIM

## View Presentation

You can view or download the prebuilt **DyadicDataAnalysis.html** presentation:

- ✅ **[Open in browser](https://pascal-kueng.github.io/05DyadicDataAnalysis/DyadicDataAnalysis.html)**
- 💾 **[Download file (rightclick --> save as](https://github.com/Pascal-Kueng/05DyadicDataAnalysis/raw/main/DyadicDataAnalysis.html)**

- ✅ <a href="https://pascal-kueng.github.io/05DyadicDataAnalysis/DyadicDataAnalysis.html" target="_blank">Open in browser</a>
- 💾 <a href="https://github.com/Pascal-Kueng/05DyadicDataAnalysis/raw/main/DyadicDataAnalysis.html" download>Download file</a>


---


## Reproduce everything locally

If you want to run all analyses or rebuild the slides locally:

1. **Clone** the repo to your computer (e.g., [using GitHub Desktop](https://desktop.github.com/download/)).
2. **Open** the `Dyadic Multilevel Modelling.Rproj`.
3. **Open** the presentation `.Rmd`.
4. Install project packages:

   ```r
   renv::restore()
   ```
5. (Recommended: Set up the use of `cmdstanr` backend for `brms`)

   ```r
   install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
   cmdstanr::check_cmdstan_toolchain(fix=TRUE)
   cmdstanr::install_cmdstan()
   ```
6. Run interactively or build the slides ("render" button at the top)

