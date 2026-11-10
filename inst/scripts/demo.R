# ST4 demo script
#
# Shows how to use the package, step by step. Edit the settings below and run
# the whole script. Then look at the results, change the settings and run it
# again.
#
# To get your own copy of this script to edit:
#   file.copy(system.file("scripts", "demo.R", package = "ST4"), "demo.R")

library(ST4)

# ---- Settings ----------------------------------------------------------------

# The two input files. These point at the synthetic example data that comes
# with the package. To use your own data, replace them with the paths to
# your files, for example "C:/surveys/my_counts.csv".
counts_file   <- system.file("extdata", "synthetic_counts.csv", package = "ST4")
metadata_file <- system.file("extdata", "synthetic_metadata.csv", package = "ST4")

# The stratum to look at, and the metadata column it is in
stratum    <- "sand"
stratum_by <- "Substratum"

# The metrics to calculate. Built in: richness, abundance, density, shannon,
# simpson, invsimpson, pielou. See ?su_metrics for how to add others.
metrics <- c("richness", "density", "shannon")

# For the number of sample units: your best guess at the coefficient of
# variation (CV, %) of the metric, and the smallest difference between
# strata you want to be able to detect (delta, % of the mean)
guessed_cv <- 20
delta      <- 40

# Settings for the number of sample units. Every setting has a default, so
# only write the ones you want to change, for example
# sample_size_control(alpha = 0.1, power = 0.8). Run sample_size_control()
# on its own to see them all.
sample_size_settings <- sample_size_control()


# ---- 1. Load the data --------------------------------------------------------

# Reads both files and joins them by tile id
survey <- read_counts(counts_file, metadata_file)
survey

# Counts for the first few tiles and taxa
survey$comm[1:5, 1:6]


# ---- 2. Pick one stratum -----------------------------------------------------

one_stratum <- select_stratum(survey, stratum, by = stratum_by)
one_stratum


# ---- 3. Metrics for each tile ------------------------------------------------

# Calculated by vegan. One row per tile.
tile_metrics <- su_metrics(one_stratum, metrics)
head(tile_metrics)
summary(tile_metrics[metrics])


# ---- 4. Number of sample units needed ----------------------------------------

# Sample units needed per stratum for the guessed CV and delta
sample_size(cv = guessed_cv, delta = delta, control = sample_size_settings)

# The same for a range of CVs and deltas. With the default settings this
# recreates Table 1 of Durden et al. (2016).
needed <- sample_size_table(control = sample_size_settings)
needed

# As a heatmap. Cells needing more than 10 sample units are greyed out, as in
# the paper.
plot(needed, max_shown = 10)


# ---- Coming later ------------------------------------------------------------
# These steps will be added as the package grows:
#   - build composite sample units within the stratum
#   - sample unit size curves and their diagnostics
#   - number of sample units from the CV of your own data
