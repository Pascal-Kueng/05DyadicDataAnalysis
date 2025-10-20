
# Dyadic Multilevel Modelling 

## View Presentation

You can **download and open the prebuilt DyadicDataAnalysis.html** presentation in your browser to view the slides. 

---

## Reproduce everything locally

If you want to run all analyses or rebuild the slides locally:

1. **Clone** the repo.
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

