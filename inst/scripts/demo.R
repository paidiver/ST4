# ST4 demo script
#
# Shows how to use the package, step by step, the way a survey is usually
# planned: start from a guess, then replace it with better estimates as the
# data comes in. The estimates are tracked in a table as you go.
#
# Edit the settings below and run the whole script. Then look at the results,
# change the settings and run it again.
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

# Describe or cite your data. This is shown on every result, so it is always
# clear where the data came from.
data_citation <- "Synthetic example data from the ST4 package (made-up values)"

# The stratum to look at, and the metadata column it is in
stratum    <- "sand"
stratum_by <- "Substratum"

# The metrics to calculate. Built in: richness, abundance, density, shannon,
# simpson, invsimpson, pielou. See ?su_metrics for how to add others.
metrics <- c("richness", "density", "shannon")

# For the number of sample units: your best guess at the coefficient of
# variation (CV, %) of the metric, and the smallest difference between
# strata you want to be able to detect (delta, % of the mean). The guess is
# only a starting point. It is replaced by CVs from the data in steps 4 and 6.
guessed_cv <- 25
delta      <- 40

# The CVs (%) and deltas (%) to show in the table and heatmap. Your guessed CV
# and delta should be in these lists, so they can be outlined on the heatmap.
# Leave both out of sample_size_table() and set power = 0.9 and min_n = 2 to
# get Table 1 of Durden et al. (2016).
table_cv    <- seq(10, 30, 5)
table_delta <- c(20, 30, 40, 80, 100)

# Settings for the number of sample units. Every setting has a default, so
# only write the ones you want to change, for example
# sample_size_control(alpha = 0.1, power = 0.8). Run sample_size_control()
# on its own to see them all.
sample_size_settings <- sample_size_control()

# How to build composite sample units (SUs) from images: "area" (m² of
# seabed), "individuals" or "images". The SU size itself is chosen in step 5,
# after looking at the curves.
su_type <- "area"

# Settings for the sample unit size curves. By default images are drawn in
# 1000 random orders, with replacement. Run su_curve_control() on its own to
# see them all.
su_curve_settings <- su_curve_control()

# Seed for the random steps (curves and grouping images into SUs). The same
# seed gives the same results.
seed <- 1


# ---- 1. Load the data --------------------------------------------------------

# Reads both files and joins them by image id
survey <- read_counts(counts_file, metadata_file,
                      data_citation = data_citation)
survey

# Counts for the first few images and taxa
survey$comm[1:5, 1:6]


# ---- 2. Pick one stratum -----------------------------------------------------

one_stratum <- select_stratum(survey, stratum, by = stratum_by)
one_stratum

# A quick check of the data: empty images, individuals and area per image,
# rare and common taxa
summary(one_stratum)


# ---- 3. Metrics for each image ------------------------------------------------

# Calculated by vegan. One row per image.
image_metrics <- su_metrics(one_stratum, metrics)
head(image_metrics)

# The values that matter for the survey: CV, zeros and the most variable
# metric
summary(image_metrics)

# How each metric is spread across the images, with the mean marked
plot(image_metrics)


# ---- 4. Number of sample units needed ----------------------------------------

# Start from the guess. cv_estimate and n_needed are updated as better
# estimates come in, and each one is added to the estimates table.
cv_estimate <- guessed_cv
n_needed <- sample_size(cv = cv_estimate, delta = delta,
                        control = sample_size_settings)
n_needed

estimates <- data.frame(stage = "Guess", units = "not known yet",
                        metric = "any", cv = cv_estimate, n_needed = n_needed)

# The same for the range of CVs and deltas set above
needed <- sample_size_table(cv = table_cv, delta = table_delta,
                            control = sample_size_settings)
needed

# As a heatmap. Cells needing more than 10 sample units are greyed out, as in
# the paper. Your guessed CV and delta are outlined, and the panel on the
# right lists the settings used.
plot(needed, max_shown = 10, highlight = c(cv = guessed_cv, delta = delta))

# Now replace the guess with CVs worked out from the data for each metric.
# Different metrics vary by different amounts, so the survey should be
# designed for the one that needs the most sample units (the worst case).
from_images <- sample_size_data(one_stratum, delta = delta, metrics = metrics,
                               control = sample_size_settings)
from_images
plot(from_images)

worst <- which.max(from_images$cv)
cv_estimate <- from_images$cv[worst]
n_needed <- from_images[[paste0("n_delta_", delta)]][worst]
estimates <- rbind(estimates, data.frame(
  stage = "Single images", units = "1 image", metric = from_images$metric[worst],
  cv = cv_estimate, n_needed = n_needed
))
estimates

# Single images vary a lot, so they need many sample units. Pooling images
# into bigger SUs smooths this out. Steps 5 and 6 find a good SU size.


# ---- 5. Sample unit size curves ----------------------------------------------

# How each metric changes as more images are pooled into one SU. Where a curve
# flattens out, adding more images to an SU changes the metric little, which
# helps choose the SU size for step 6.
curves <- su_curve(one_stratum, metrics, control = su_curve_settings,
                   seed = seed)
curves

# One panel per metric. The line is the median over the random orders and the
# band holds the middle 95% of them. Seabed area is along the bottom and
# individuals along the top. Try x_axis = c("images", "area") to show the
# number of images instead.
plot(curves)

# All the metrics in one panel, each as a % of its value at the last step.
# The metric that takes longest to reach 100% needs the biggest SUs.
plot(curves, compare = TRUE, x_axis = c("images", "area"))

# Decision: read the SU size off the curves, in the units of su_type. Here
# the curves have mostly flattened out and the bands narrowed by about
# 150 m². Change this and run the steps below again to compare sizes.
su_size <- 150


# ---- 6. Composite sample units -----------------------------------------------

# Groups the images into SUs of the chosen size at random. Leftover images that
# do not fill an SU are dropped.
sus <- build_sample_units(one_stratum, type = su_type, size = su_size,
                          seed = seed)
sus

# The same check for the SUs
summary(sus)

# The images in each SU
head(sus$members)

# The same summary for the SUs. No zeros, and much lower CVs
summary(su_metrics(sus, metrics))

# Update the estimates with the CVs of the SUs
from_sus <- sample_size_data(sus, delta = delta, metrics = metrics,
                             control = sample_size_settings)
from_sus
plot(from_sus)

# How much less the metrics vary in SUs than in single images
plot(metric_cv(one_stratum, metrics), metric_cv(sus, metrics))

worst <- which.max(from_sus$cv)
cv_estimate <- from_sus$cv[worst]
n_needed <- from_sus[[paste0("n_delta_", delta)]][worst]
estimates <- rbind(estimates, data.frame(
  stage = "Composite SUs",
  units = paste(su_size, c(area = "m²", individuals = "individuals",
                           images = "images")[[su_type]]),
  metric = from_sus$metric[worst], cv = cv_estimate, n_needed = n_needed
))


# ---- 7. Survey plan ----------------------------------------------------------

# How the estimates changed from the first guess to the SUs
estimates$cv <- round(estimates$cv, 1)
estimates

# What the final estimate means for the survey, per stratum. Most of the time
# what matters is how many images to take, as that sets the ship time.
images_per_su <- mean(sus$meta$n_images)
area_per_su  <- mean(sus$meta[[sus$settings$area]])
cat(n_needed, " SUs per stratum, each about ", round(images_per_su),
    " images (", round(area_per_su), " m²).\n",
    "That is about ", ceiling(n_needed * images_per_su), " images and ",
    round(n_needed * area_per_su), " m² of seabed per stratum.\n", sep = "")


# ---- Coming later ------------------------------------------------------------
# These steps will be added as the package grows:
#   - diagnostics for where the curves flatten out
