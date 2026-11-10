# Small made-up data where the answers can be worked out by hand.
#
#   tile  a  b  c  d   area (m²)
#   t1    3  1  0  0   2
#   t2    0  0  0  0   5
#   t3    1  1  1  1   8
#   t4    5  0  0  0   10
#
# Richness per tile is 2, 0, 4, 1.
cv_data <- function() {
  counts <- data.frame(
    tile = c("t1", "t2", "t3", "t4"),
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

test_that("metric_cv leaves out units where a metric is NA", {
  out <- metric_cv(cv_data(), c("richness", "shannon"))
  # Shannon is NA for the empty tile t2, so only 3 tiles are used
  expect_equal(out$n_units, c(4, 3))
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
  expect_output(print(out), "alpha: 0.05")
})

test_that("works on the synthetic data", {
  files <- c(system.file("extdata", "synthetic_counts.csv", package = "ST4"),
             system.file("extdata", "synthetic_metadata.csv", package = "ST4"))
  x <- select_stratum(read_counts(files[1], files[2]), "hard")
  out <- sample_size_data(x, delta = 40, metrics = c("richness", "density"))
  expect_equal(nrow(out), 2)
  expect_true(all(out$n_delta_40 >= 2, na.rm = TRUE))
})
