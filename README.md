
<!-- README.md is generated from README.Rmd. Please edit that file -->

# HeliconiaDemography

<!-- badges: start -->

[![Run
Tests](https://github.com/BrunaLab/HeliconiaDemography/actions/workflows/run-testthat.yaml/badge.svg)](https://github.com/BrunaLab/HeliconiaDemography/actions/workflows/run-testthat.yaml)
[![Zenodo
DOI](https://zenodo.org/badge/289078614.svg)](https://zenodo.org/badge/latestdoi/289078614)
[![Paper
DOI](https://img.shields.io/badge/DOI-10.1111%2Fgcb.15900-blue)](https://doi.org/10.1111/gcb.15900)

<!-- badges: end -->

This repository contains the data and code for:

Scott ER, Uriarte M, Bruna EM (2021) Delayed effects of climate on vital
rates lead to demographic divergence in Amazonian forest fragments.
Global Change Biology. <https://doi.org/10.1111/gcb.15900>

## How to cite

If you use the data or code in this repository, please cite the [Zenodo
archive](https://zenodo.org/badge/latestdoi/289078614) as well as the
Global Change Biology [paper](https://doi.org/10.1111/gcb.15900).

## How to run

This research compendium has been developed using the statistical
programming language R. To work with the compendium, you will need
installed on your computer the [R
software](https://cloud.r-project.org/) itself and optionally [RStudio
Desktop](https://rstudio.com/products/rstudio/download/).

You can download the compendium as a zip from from this URL:
[master.zip](/archive/master.zip).

To run the compendium and reproduce all outputs:

-   Open the project in RStudio by double-clicking the
    `HeliconiaDemography.Rproj` file.
-   Install the [`renv`
    package](https://rstudio.github.io/renv/articles/renv.html) if it’s
    not already installed and run `renv::restore()` to install all
    package dependencies.
-   Install the [`targets` package](https://docs.ropensci.org/targets/).
-   Run `targets::tar_make()` or `targets::tar_make_clustermq()` from
    the R console to run all code and produce all outputs.

## Additional Dependencies

-   Numbering of and in-text references to equations in `paper.Rmd` is
    handled by
    [`pandoc-crossref`](https://github.com/lierdakil/pandoc-crossref),
    which may not be part of the standard RStudio pandoc installation.
    You might need to install it for the main text to knit correctly to
    Word.
