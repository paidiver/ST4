# Durden et al. (2016) Table 1 gives the SUs required per stratum for 2 strata
# with alpha 0.05 and power 0.9. Rows are CV % and columns are delta %.
# NA marks the grey cells in the paper, where more than 10 are needed.
durden_table1 <- matrix(c(
   7,  4,  3,  3,  2,  2,  2,  2,  2,  2,
  NA, NA,  7,  4,  3,  3,  2,  2,  2,  2,
  NA, NA, NA,  8,  5,  3,  3,  2,  2,  2,
  NA, NA, NA, NA,  7,  4,  3,  3,  2,  2,
  NA, NA, NA, NA, 10,  6,  4,  3,  2,  2,
  NA, NA, NA, NA, NA,  8,  5,  3,  3,  2,
  NA, NA, NA, NA, NA, 10,  6,  4,  3,  2,
  NA, NA, NA, NA, NA, NA,  7,  4,  3,  3,
  NA, NA, NA, NA, NA, NA,  8,  5,  4,  3,
  NA, NA, NA, NA, NA, NA, 10,  6,  4,  3
), nrow = 10, byrow = TRUE)
table1_cv <- seq(5, 50, 5)
table1_delta <- c(10, 14, 20, 28, 40, 56, 79, 112, 158, 224)

test_that("reproduces Durden et al. (2016) Table 1", {
  grid <- expand.grid(cv = table1_cv, delta = table1_delta)
  n <- sample_size(grid$cv, grid$delta, max_n = 1000)
  got <- matrix(n, nrow = length(table1_cv))

  shown <- !is.na(durden_table1)
  expect_equal(got[shown], durden_table1[shown])
  expect_true(all(got[!shown] > 10)) # grey cells need more than 10
})

test_that("cases the original script got wrong", {
  # The original script gave 9 and 3 here, from a one-tailed alpha and from
  # using P as the significance level
  expect_equal(sample_size(45, 79), 8L)
  expect_equal(sample_size(45, 158), 4L)
})

test_that("recycles cv and delta", {
  expect_equal(sample_size(30, c(20, 40, 80)),
               c(sample_size(30, 20), sample_size(30, 40), sample_size(30, 80)))
  expect_length(sample_size(c(10, 20), c(40, 80)), 2)
})

test_that("returns NA when more than max_n needed", {
  expect_identical(sample_size(50, 10, max_n = 10), NA_integer_)
})

test_that("more power needs more SUs, more strata (more df) needs fewer", {
  expect_gte(sample_size(30, 40, power = 0.95), sample_size(30, 40, power = 0.8))
  expect_lte(sample_size(30, 40, n_strata = 4), sample_size(30, 40, n_strata = 2))
})

test_that("rejects bad input", {
  expect_error(sample_size(-5, 40), "cv")
  expect_error(sample_size(20, 0), "delta")
  expect_error(sample_size(20, 40, alpha = 5), "alpha")
  expect_error(sample_size(20, 40, power = 1), "power")
  expect_error(sample_size(20, 40, n_strata = 1), "n_strata")
  expect_error(sample_size(20, 40, max_n = 2.5), "max_n")
})
