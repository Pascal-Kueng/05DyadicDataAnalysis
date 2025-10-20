
# Dyadic Multilevel Modelling 

## View Presentation

You can **download and open the prebuilt DyadicDataAnalysis.html** presentation in your browser to view the slides. 

---

## Reproduce everything locally

If you want to run all analyses and rebuild the slides locally:

1. **Clone** the repo.
2. **Open** the `Dyadic Multilevel Modelling.Rproj`.
3. **Open** the presentation `.Rmd`.
4. Install project packages:

   ```r
   renv::restore()
   ```
5. (Recommended: Set up the use of `cmdstanr` backend for brms)

   ```r
   install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
   cmdstanr::check_cmdstan_toolchain(fix=TRUE)
   cmdstanr::install_cmdstan()
   ```
6. **Render** run interactively or render the slides

### Data availability

* **Included:** cross-sectional data (`brms_models_cache/simulatedCrossSectionalDyadicData.rds`).
* **Not included yet:** longitudinal data (e.g., `brms_models_cache/df_long_pilot.rds`).
  Longitudinal chunks will fail without these files; cross-sectional parts will run.

