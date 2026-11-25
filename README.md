# ST4

<!-- badges: start -->
[![R-CMD-check](https://github.com/paidiver/ST4/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/paidiver/ST4/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Choose the size and number of sample units (SUs) for photographic seabed
surveys, following Durden et al. (2016). Part of the PAIDIVER project.
Metrics are calculated by vegan.

**Alpha:** the methods work, but the default settings are not confirmed yet
([#42](https://github.com/paidiver/ST4/issues/42)).

## Install

```r
# install.packages("remotes")
remotes::install_github("paidiver/ST4")
```

## Input data

Two CSV files (or data frames), joined by image id:

- **Counts:** one row per image, one column per taxon. The first column
  holds the image ids.
- **Metadata:** one row per image, with the image id (`FileName`), seabed
  area in m² (`Area_seabed_m2`) and a column for the strata, such as
  `Substratum`. Other column names can be given to `read_counts()`.

## Quick start

```r
library(ST4)

# 1. Load the data and pick one stratum
survey <- read_counts("counts.csv", "metadata.csv",
                      data_citation = "My survey, 2026")
sand <- select_stratum(survey, "sand", by = "Substratum")
summary(sand)

# 2. Metrics for each image, and how much they vary
metrics <- c("richness", "density", "shannon")
summary(su_metrics(sand, metrics))

# 3. Number of SUs needed, from a guessed CV or from the data
sample_size(cv = 25, delta = 40)
plot(sample_size_table())
plot(sample_size_data(sand, delta = 40, metrics = metrics))

# 4. SU size: how the metrics change as images are pooled
curves <- su_curve(sand, metrics, seed = 1)
plot(curves)

# 5. Build SUs of the chosen size and work out the number needed again
sus <- build_sample_units(sand, type = "area", size = 150, seed = 1)
plot(sample_size_data(sus, delta = 40, metrics = metrics))
```

Every setting has a default. Change one by giving it to the function, such
as `power = 0.8`, or see them all with `sample_size_control()`,
`sample_unit_control()` and `su_curve_control()`.

## Demos

Two scripts walk through every step. Copy one to your working folder, then
open it and run it:

```r
# Synthetic data (made-up values, see ?synthetic_data)
file.copy(system.file("scripts", "demo.R", package = "ST4"), "demo.R")

# Real data from BODC (CC BY 4.0, see ?bodc_data)
file.copy(system.file("scripts", "demo_bodc.R", package = "ST4"), "demo_bodc.R")
```

To use your own data, change the file paths at the top of the script.

## Terms

- **Image:** a single seabed photo, one row of the input.
- **Sample unit (SU):** what the survey samples. Either a single image or a
  composite SU of several images pooled together.
- **CV:** coefficient of variation of a metric across SUs, as a % of the
  mean.
- **Delta:** the smallest difference between strata to detect, as a % of
  the mean.

## Help

```r
?ST4
help(package = "ST4")
```

## Licence

Code: Apache License 2.0. The BODC example data is CC BY 4.0, see
`inst/COPYRIGHTS`.

## Reference

Durden, J.M., Schoening, T., Althaus, F., et al. (2016). Perspectives in
visual imaging for marine biology and ecology: from acquisition to
understanding. *Oceanography and Marine Biology: An Annual Review*, 54,
1-72.

## Development

```r
devtools::document() # rebuild man/ and NAMESPACE from roxygen comments
devtools::test()     # run tests
devtools::check()    # full R CMD check
```
