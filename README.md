# Distinguishable and Exchangeable Dyads: Multilevel Modelling
Cross-Sectional and Intensive Longitudinal APIM and DIM 

[![DOI](https://zenodo.org/badge/1079959998.svg)](https://doi.org/10.5281/zenodo.17400655)

## View Presentation

You can view or download the prebuilt **DyadicDataAnalysis.html** presentation:

- ✅ **[Open in browser (Ctr+Click for new tab)](https://pascal-kueng.github.io/05DyadicDataAnalysis/DyadicDataAnalysis.html)**
- 💾 **[Download zip folder with HTML file included](https://github.com/Pascal-Kueng/05DyadicDataAnalysis/releases/latest)**

---


## Reproduce everything locally

If you want to run all analyses or rebuild the slides locally:

1. [Download zip folder](https://github.com/Pascal-Kueng/05DyadicDataAnalysis/releases/latest) or clone the repository
2. **Open** the `00DyadicDataAnalysis.Rproj`.
3. **Open** the presentation `DyadicDataAnalysis.Rmd`.
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

---

If you use or adapt material from this project, please cite:

> Küng, P. (2025). *Distinguishable and Exchangeable Dyads: Multilevel Modelling*. Zenodo. https://doi.org/10.5281/zenodo.17400655

```{bibtex}
@misc{kueng_2025_dyadic,
  author       = {K{\"u}ng, Pascal},
  title        = {Distinguishable and Exchangeable Dyads: Multilevel Modelling},
  year         = {2025},
  publisher    = {Zenodo},
  doi          = {10.5281/zenodo.17400655},
  url          = {https://doi.org/10.5281/zenodo.17400655},
  note         = {Slides and analysis material}
}
```

[![License: CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)
