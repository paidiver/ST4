# durden_table1, table1_cv and table1_delta are in helper-su-matrix.R

test_that("the defaults recreate Durden et al. (2016) Table 1", {
  tab <- sample_size_table()
  m <- unclass(tab)

  expect_equal(rownames(m), as.character(table1_cv))
  expect_equal(colnames(m), as.character(table1_delta))

  shown <- !is.na(durden_table1)
  expect_equal(m[shown], durden_table1[shown])
  expect_true(all(is.na(m[!shown]) | m[!shown] > 10))
})

test_that("each cell matches sample_size() called directly", {
  tab <- sample_size_table(cv = c(10, 25, 40), delta = c(15, 50),
                           n_strata = 3, alpha = 0.1, power = 0.8)
  for (i in 1:3) {
    for (j in 1:2) {
      expect_equal(
        tab[i, j],
        sample_size(c(10, 25, 40)[i], c(15, 50)[j],
                    n_strata = 3, alpha = 0.1, power = 0.8)
      )
    }
  }
})

test_that("keeps the settings used", {
  tab <- sample_size_table(n_strata = 3, alpha = 0.1, power = 0.8, max_n = 50)
  expect_s3_class(tab, "sample_size_table")
  expect_equal(attr(tab, "settings"),
               list(n_strata = 3, alpha = 0.1, power = 0.8, max_n = 50))
})

test_that("print shows the settings and marks cells over max_n", {
  tab <- sample_size_table(cv = c(5, 50), delta = c(10, 100), max_n = 20)
  expect_output(print(tab), "alpha: 0.05, power: 0.9")
  expect_output(print(tab), ">20")
})

test_that("plot draws without error and returns the table", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  tab <- sample_size_table()

  expect_invisible(plot(tab))
  expect_identical(plot(tab, max_shown = 10), tab)
  # Every cell greyed out
  expect_no_error(plot(sample_size_table(cv = 50, delta = 10, max_n = 5)))
  expect_error(plot(tab, max_shown = 0), "max_shown")
})
