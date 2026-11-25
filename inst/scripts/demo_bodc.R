# ST4 demo script on real data from BODC
#
# The same steps as demo.R, on real seabed image annotations from the
# abyssal Pacific (see ?bodc_data for the source and licence). Start from a
# guess, then replace it with better estimates as the data comes in. The
# estimates are tracked in a table as you go.
#
# Limits of this data (see ?bodc_data):
#   - No image areas are published, so each image is given 1 m². Density is
#     individuals per image, and SUs are built by number of images.
#   - Only images with annotations are included, so there are no empty
#     images. A real survey would have more variable images and need more
#     SUs.
#   - It was collected to compare observers, so it is small: 67 images.
#
# To get your own copy of this script to edit:
#   file.copy(system.file("scripts", "demo_bodc.R", package = "ST4"),
#             "demo_bodc.R")

library(ST4)

# ---- Settings ----------------------------------------------------------------

# The two input files, from BODC
counts_file   <- system.file("extdata", "bodc_counts.csv", package = "ST4")
metadata_file <- system.file("extdata", "bodc_metadata.csv", package = "ST4")

# Shown on every result. CC BY 4.0: the data creators and BODC must be
# credited.
data_citation <- paste(
  "Durden, Bingo and Putts (2026), BODC,",
  "doi:10.5285/5f9b8ce1-0b4d-4061-e063-7086abc0b131 (CC BY 4.0)"
)

# All the images are from one cruise, used as one stratum
stratum    <- "KM1808"
stratum_by <- "Survey"

# The metrics to calculate. Built in: richness, abundance, density, shannon,
# simpson, invsimpson, pielou. See ?su_metrics for how to add others.
metrics <- c("richness", "density", "shannon")

# For the number of sample units: your best guess at the coefficient of
# variation (CV, %) of the metric, and the smallest difference between
# strata you want to be able to detect (delta, % of the mean)
guessed_cv <- 25
delta      <- 40

# The CVs (%) and deltas (%) to show in the table and heatmap
table_cv    <- seq(10, 30, 5)
table_delta <- c(20, 30, 40, 80, 100)

# Settings for the number of sample units. Run sample_size_control() on its
# own to see them all.
sample_size_settings <- sample_size_control()

# How to build composite sample units (SUs) from images. There are no image
# areas in this data, so SUs are built from a number of images.
su_type <- "images"

# Settings for the sample unit size curves. Run su_curve_control() on its own
# to see them all.
su_curve_settings <- su_curve_control()

# Seed for the random steps. The same seed gives the same results.
seed <- 1


# ---- 1. Load the data --------------------------------------------------------

survey <- read_counts(counts_file, metadata_file,
                      data_citation = data_citation)
survey

# Counts for the first few images and labels. Image ids are the date and
# time each image was taken.
survey$comm[1:5, 1:6]

# What the labels are, and whether each is in the published label set
labels <- read.csv(system.file("extdata", "bodc_labels.csv", package = "ST4"))
head(labels)


# ---- 2. Pick one stratum -----------------------------------------------------

one_stratum <- select_stratum(survey, stratum, by = stratum_by)

# A quick check of the data: individuals per image, rare and common taxa
summary(one_stratum)


# ---- 3. Metrics for each image ------------------------------------------------

image_metrics <- su_metrics(one_stratum, metrics)
head(image_metrics)
summary(image_metrics)
plot(image_metrics)


# ---- 4. Number of sample units needed ----------------------------------------

# Start from the guess
cv_estimate <- guessed_cv
n_needed <- sample_size(cv = cv_estimate, delta = delta,
                        control = sample_size_settings)
n_needed

estimates <- data.frame(stage = "Guess", units = "not known yet",
                        metric = "any", cv = cv_estimate, n_needed = n_needed)

needed <- sample_size_table(cv = table_cv, delta = table_delta,
                            control = sample_size_settings)
needed
plot(needed, max_shown = 10, highlight = c(cv = guessed_cv, delta = delta))

# Replace the guess with CVs from the images, and design for the worst case
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


# ---- 5. Sample unit size curves ----------------------------------------------

curves <- su_curve(one_stratum, metrics, control = su_curve_settings,
                   seed = seed)
curves

# Number of images along the bottom, as the areas are not real
plot(curves, x_axis = c("images", "individuals"))
plot(curves, compare = TRUE, x_axis = c("images", "individuals"))

# Decision: read the SU size off the curves. Shannon flattens by about 20 to
# 30 images and richness keeps climbing. With only 67 images, SUs of 10
# images give 6 SUs, enough to work out a CV. Bigger SUs would give too few.
su_size <- 10


# ---- 6. Composite sample units -----------------------------------------------

sus <- build_sample_units(one_stratum, type = su_type, size = su_size,
                          seed = seed)
sus
summary(sus)
head(sus$members)
summary(su_metrics(sus, metrics))

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
  stage = "Composite SUs", units = paste(su_size, "images"),
  metric = from_sus$metric[worst], cv = cv_estimate, n_needed = n_needed
))


# ---- 7. Survey plan ----------------------------------------------------------

estimates$cv <- round(estimates$cv, 1)
estimates

# In images only, as there are no image areas
images_per_su <- mean(sus$meta$n_images)
cat(n_needed, " SUs per stratum, each about ", round(images_per_su),
    " images.\n", "That is about ", ceiling(n_needed * images_per_su),
    " images per stratum.\n", sep = "")
