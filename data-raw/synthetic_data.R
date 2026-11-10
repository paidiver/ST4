# Builds the synthetic example data in inst/extdata/.
# Made-up values only. See ?synthetic_data for how they are made.
# Run from the package root: source("data-raw/synthetic_data.R")

devtools::load_all()

dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)
write_synthetic_data(make_synthetic_data(), "inst/extdata")
