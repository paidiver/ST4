# Small made-up data where the answers can be worked out by hand.
#
#   image  a  b  c  d   area (m²)
#   t1    3  1  0  0   2
#   t2    0  0  0  0   5
#   t3    1  1  1  1   8
#   t4    5  0  0  0   10
#
# Richness per image is 2, 0, 4, 1.
cv_data <- function() {
  counts <- data.frame(
    image = c("t1", "t2", "t3", "t4"),
    a = c(3, 0, 1, 5),
    b = c(1, 0, 1, 0),
    c = c(0, 0, 1, 0),
    d = c(0, 0, 1, 0)
  )
  metadata <- data.frame(
    FileName = c("t1", "t2", "t3", "t4"),
    Area_seabed_m2 = c(2, 5, 8, 10)
  )
  read_counts(counts, metadata)
}

test_that("metric_cv gives the right answer on known data", {
  out <- metric_cv(cv_data())
  expect_equal(out$metric, "richness")
  expect_equal(out$n_units, 4)

  # mean = (2 + 0 + 4 + 1) / 4 = 7 / 4 = 1.75
  # deviations from the mean: 0.25, -1.75, 2.25, -0.75
  # squared: 0.0625 + 3.0625 + 5.0625 + 0.5625 = 8.75
  # sample variance = 8.75 / (4 - 1) = 2.9167, sd = sqrt(2.9167) = 1.7078
  # CV = 100 * 1.7078 / 1.75 = 97.59 %
  expect_equal(out$mean, 1.75)
  expect_equal(out$sd, sqrt(8.75 / 3))
  expect_equal(out$cv, 100 * sqrt(8.75 / 3) / 1.75)
})

test_that("metric_cv leaves out units where a metric is not finite", {
  out <- suppressWarnings(
    metric_cv(cv_data(), c("richness", "shannon", "invsimpson"))
  )
  # The empty image t2 has Shannon 0, which is kept, so 4 images are used.
  # Its inverse Simpson is 1 / 0 = Inf, so only 3 images are used for that.
  expect_equal(out$n_units, c(4, 4, 3))
  expect_true(is.finite(out$cv[3]))
  expect_output(print(out), "left out for that metric")
  # Richness uses all 4 images, so there is no note
  printed <- utils::capture.output(print(metric_cv(cv_data())))
  expect_false(any(grepl("left out", printed)))
})

test_that("metric_cv gives NA where the CV cannot be calculated", {
  x <- cv_data()
  # Only t2 is left: mean richness 0, and only 1 unit, so no sd or CV
  x$comm <- x$comm[2, , drop = FALSE]
  x$meta <- x$meta[2, , drop = FALSE]
  out <- metric_cv(x)
  expect_equal(out$mean, 0)
  expect_true(is.na(out$cv))
})

test_that("sample_size_data matches sample_size() with the CV", {
  x <- cv_data()
  out <- sample_size_data(x, delta = c(50, 100))
  cv <- metric_cv(x)$cv

  expect_s3_class(out, "sample_size_data")
  expect_equal(out$n_delta_50, sample_size(cv, 50))
  expect_equal(out$n_delta_100, sample_size(cv, 100))
})

test_that("sample_size_data takes settings like sample_size()", {
  x <- cv_data()
  cv <- metric_cv(x)$cv
  by_arg <- sample_size_data(x, delta = 100, power = 0.8)
  by_control <- sample_size_data(x, delta = 100,
                                 control = sample_size_control(power = 0.8))

  expect_equal(by_arg$n_delta_100, by_control$n_delta_100)
  expect_equal(by_arg$n_delta_100, sample_size(cv, 100, power = 0.8))
  expect_equal(attr(by_arg, "settings")$power, 0.8)
  expect_error(sample_size_data(x, delta = 100, powr = 0.8), "power")
})

test_that("print flags the metric with the highest CV", {
  x <- cv_data()
  out <- sample_size_data(x, delta = 100,
                          metrics = c("richness", "abundance", "density"))
  cv <- metric_cv(x, c("richness", "abundance", "density"))
  worst <- cv$metric[which.max(cv$cv)]

  expect_output(print(out), paste0("Worst case: ", worst))
  expect_output(print(out), "2 strata compared, alpha 0.05")
  # cv_data() has 4 images and no stratum selected
  expect_output(print(out), "Data: 4 images, stratum all \\(none selected\\)")
})

test_that("metric_cv and sample_size_data say which data they came from", {
  files <- c(system.file("extdata", "synthetic_counts.csv", package = "ST4"),
             system.file("extdata", "synthetic_metadata.csv", package = "ST4"))
  x <- select_stratum(read_counts(files[1], files[2]), "hard")
  # The synthetic data has 30 images in the hard substratum
  label <- "Data: 30 images, stratum Substratum = hard"
  expect_output(print(metric_cv(x)), label)
  expect_output(print(sample_size_data(x, delta = 40)), label)
})

test_that("works on the synthetic data", {
  files <- c(system.file("extdata", "synthetic_counts.csv", package = "ST4"),
             system.file("extdata", "synthetic_metadata.csv", package = "ST4"))
  x <- select_stratum(read_counts(files[1], files[2]), "hard")
  out <- sample_size_data(x, delta = 40, metrics = c("richness", "density"))
  expect_equal(nrow(out), 2)
  expect_true(all(out$n_delta_40 >= 2, na.rm = TRUE))
})

# Three substrata, for counting the strata in the data
strata_cv_data <- function() {
  counts <- data.frame(
    image = paste0("t", 1:6),
    a = c(3, 0, 1, 5, 2, 2),
    b = c(1, 2, 1, 0, 0, 1)
  )
  metadata <- data.frame(
    FileName = paste0("t", 1:6),
    Area_seabed_m2 = rep(5, 6),
    Substratum = c("sand", "sand", "sand", "sand", "hard", "mud")
  )
  read_counts(counts, metadata)
}

test_that("the number of strata comes from the data", {
  x <- select_stratum(strata_cv_data(), "sand")
  # sand, hard and mud: 3 strata
  expect_equal(x$settings$n_strata, 3)
  out <- sample_size_data(x, delta = 40)
  expect_equal(attr(out, "settings")$n_strata, 3)
  expect_equal(out$n_delta_40, sample_size(out$cv, 40, n_strata = 3))
  expect_output(print(out), "3 strata compared \\(from the data\\)")
})

test_that("n_strata set by the user wins over the data", {
  x <- select_stratum(strata_cv_data(), "sand")
  out <- sample_size_data(x, delta = 40, n_strata = 2)
  expect_equal(attr(out, "settings")$n_strata, 2)
  expect_output(print(out), "2 strata compared, alpha")
  out <- sample_size_data(x, delta = 40,
                          control = sample_size_control(n_strata = 4))
  expect_equal(attr(out, "settings")$n_strata, 4)
})

test_that("the n_strata setting is used without a stratum", {
  # No stratum picked, so the number of strata is not known
  out <- sample_size_data(cv_data(), delta = 40)
  expect_equal(attr(out, "settings")$n_strata, 2)
  expect_false(attr(out, "n_strata_from_data"))
})

test_that("composite SUs keep the number of strata", {
  x <- select_stratum(strata_cv_data(), "sand")
  sus <- build_sample_units(x, type = "images", size = 2, seed = 1)
  expect_equal(sus$settings$n_strata, 3)
})

test_that("delta must be at least 0.1", {
  expect_error(sample_size_data(cv_data(), delta = 0), "at least 0.1")
})
