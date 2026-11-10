# ST4

<!-- badges: start -->
[![R-CMD-check](https://github.com/paidiver/ST4/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/paidiver/ST4/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

As yet unnamed ST4 Work Package for PAIDIVER.

Sample unit size and number of sample units for photographic seabed surveys. Metrics are calculated by vegan.

## Install

```r
# install.packages("remotes")
remotes::install_github("paidiver/ST4")
```

## Demo

A demo script walks through the package on synthetic example data. Copy it
to your working folder, then open it and run it:

```r
file.copy(system.file("scripts", "demo.R", package = "ST4"), "demo.R")
```

To use your own data, change the file paths in the settings at the top of the
script.

## Development

```r
devtools::document() # rebuild man/ and NAMESPACE from roxygen comments
devtools::test()     # run tests
devtools::check()    # full R CMD check
```
