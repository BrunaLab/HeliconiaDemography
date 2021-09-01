
<!-- README.md is generated from README.Rmd. Please edit that file -->

# HeliconiaDemography

<!-- badges: start -->

[![Run
Tests](https://github.com/BrunaLab/HeliconiaDemography/actions/workflows/run-testthat.yaml/badge.svg)](https://github.com/BrunaLab/HeliconiaDemography/actions/workflows/run-testthat.yaml)

<!-- badges: end -->

This repository contains the data and code for a manuscript written for
submission to Global Change Biology tentatively titled: “Delayed effects
of climate on vital rates leads to demographic divergence in Amazonian
forest fragments”

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

**Using HPC**: You may need to install packages on the remote HPC if
using `tar_make_clustermq()` with the SSH option. Unfortunately, I’m not
sure if there is an easy way to do this using `renv`. You should,
however, make sure the same versions of the packages are installed
locally and on the HPC, especially `gratia` which I’m using a
development version of that has important bugs fixed, but not new bugs
introduced.

## Additional Dependencies

-   Numbering of and in-text references to equations in `paper.Rmd` is
    handled by
    [`pandoc-crossref`](https://github.com/lierdakil/pandoc-crossref),
    which may not be part of the standard RStudio pandoc installation.
    You might need to install it for the main text to knit correctly to
    Word.

<!--
### Licenses
-->
<!-- **Text and figures :** [CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/) -->
<!-- <!-- **Code :** See the [DESCRIPTION](DESCRIPTION) file -->
<!-- **Data :** [CC-0](http://creativecommons.org/publicdomain/zero/1.0/) attribution requested in reuse -->
