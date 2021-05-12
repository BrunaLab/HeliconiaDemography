
<!-- README.md is generated from README.Rmd. Please edit that file -->

# HeliconiaDemography

<!-- badges: start -->

[![Run
Tests](https://github.com/BrunaLab/HeliconiaDemography/actions/workflows/run-testthat.yaml/badge.svg)](https://github.com/BrunaLab/HeliconiaDemography/actions/workflows/run-testthat.yaml)

<!-- badges: end -->

This repository contains the data and code for our paper:

> Authors, (YYYY). *Title of your paper goes here*. Name of journal/book
> <https://doi.org/xxx/xxx>

<!-- Our pre-print is online here: -->
<!-- > Authors, (YYYY). *Title of your paper goes here*. Name of journal/book, Accessed 10 May 2021. Online at <https://doi.org/xxx/xxx> -->

### How to cite

Please cite this compendium as:

> Authors, (2021). *Compendium of R code and data for Title of your
> paper goes here*. Accessed 10 May 2021. Online at
> <https://doi.org/xxx/xxx>

## How to run in your browser or download and run locally

This research compendium has been developed using the statistical
programming language R. To work with the compendium, you will need
installed on your computer the [R
software](https://cloud.r-project.org/) itself and optionally [RStudio
Desktop](https://rstudio.com/products/rstudio/download/).

<!-- The simplest way to explore the text, code and data is to click on [binder](https://mybinder.org/v2/gh/BrunaLab/HeliconiaDemography/master?urlpath=rstudio) to open an instance of RStudio in your browser, which will have the compendium files ready to work with. -->
<!-- Binder uses rocker-project.org Docker images to ensure a consistent and reproducible computational environment. -->
<!-- These Docker images can also be used locally. -->

You can download the compendium as a zip from from this URL:
[master.zip](/archive/master.zip).

To run the compendium and reproduce all outputs:

-   Open the `.Rproj` file in RStudio
-   Install the `renv` package if it’s not already installed and run
    `renv::restore()` to install all package dependencies. (I’m not
    actually sure that this step is necessar. Should auto-load because
    of .Rprofile.)
-   Run `targets::tar_make()` to run all code and produce all outputs.

## Notes

-   Equation numbering and in-text references in `paper.Rmd` is handled
    by [`pandoc-crossref`](https://github.com/lierdakil/pandoc-crossref)
    which I don’t think is part of the standard RStudio pandoc
    installation. You might need to install it for the main text to knit
    correctly to Word.

### Licenses

<!-- **Text and figures :** [CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/) -->
<!-- <!-- **Code :** See the [DESCRIPTION](DESCRIPTION) file -->
<!-- **Data :** [CC-0](http://creativecommons.org/publicdomain/zero/1.0/) attribution requested in reuse -->
