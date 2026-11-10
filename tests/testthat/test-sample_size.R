# durden_table1, table1_cv and table1_delta are in helper-su-matrix.R

test_that("reproduces Durden et al. (2016) Table 1", {
  grid <- expand.grid(cv = table1_cv, delta = table1_delta)
  n <- sample_size(grid$cv, grid$delta)
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

test_that("has no upper limit by default", {
  # Durden's grey cells come out as real numbers, e.g. CV 50 and delta 10
  n <- sample_size(50, 10)
  expect_false(is.na(n))
  expect_gt(n, 100)
})

test_that("the faster search start gives the same answers as starting at 2", {
  # Plain search from n = 2, as in the original script
  from_two <- function(cv, delta, alpha, power, n_strata) {
    n <- 2
    repeat {
      v <- n_strata * (n - 1)
      rhs <- 2 * (cv / delta)^2 *
        (stats::qt(1 - alpha / 2, v) + stats::qt(power, v))^2
      if (n >= rhs) return(as.integer(n))
      n <- n + 1
    }
  }
  # Includes power below 0.5, where the faster start is not used
  for (s in list(c(0.05, 0.9, 2), c(0.1, 0.8, 3), c(0.01, 0.95, 2),
                 c(0.05, 0.3, 2))) {
    for (cv in c(5, 20, 50)) {
      for (delta in c(10, 40, 224)) {
        expect_equal(
          sample_size(cv, delta, alpha = s[1], power = s[2], n_strata = s[3]),
          from_two(cv, delta, s[1], s[2], s[3])
        )
      }
    }
  }
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
