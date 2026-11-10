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
  expect_s3_class(attr(tab, "settings"), "sample_size_control")
  expect_equal(unclass(attr(tab, "settings"))[c("n_strata", "alpha", "power",
                                                "max_n")],
               list(n_strata = 3, alpha = 0.1, power = 0.8, max_n = 50))
})

test_that("print shows the settings and marks cells over max_n", {
  tab <- sample_size_table(cv = c(5, 50), delta = c(10, 100), max_n = 20)
  expect_output(print(tab), "2 strata compared, alpha 0.05, power 0.9")
  expect_output(print(tab), "not from survey data")
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

test_that("plot shows the run settings panel, and can hide it", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  tab <- sample_size_table()

  expect_no_error(plot(tab, max_shown = 10))
  expect_no_error(plot(tab, settings_panel = FALSE))
  # The two-panel layout is undone afterwards, so the next plot is normal
  plot(tab)
  expect_equal(graphics::par("mfrow"), c(1, 1))
})

test_that("plot takes names for the strata compared", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  tab <- sample_size_table()

  expect_no_error(plot(tab, strata = c("sand", "hard")))
  expect_no_error(plot(tab, strata = c("hard + coarse", "sand + hard")))
  # The table was made for 2 strata, so 3 names do not fit
  expect_error(plot(tab, strata = c("sand", "hard", "coarse")),
               "n_strata = 3")
  expect_error(plot(tab, strata = 1:2), "names")
})

test_that("plot highlights a CV and delta", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  tab <- sample_size_table()

  # On the grid: CV 20 is a row and delta 40 a column of the default table
  expect_no_error(plot(tab, highlight = c(cv = 20, delta = 40)))
  # Off the grid: still allowed, the panel shows the worked-out value
  expect_no_error(plot(tab, highlight = c(delta = 33, cv = 22)))
  expect_error(plot(tab, highlight = c(20, 40)), "cv = 20, delta = 40")
  expect_error(plot(tab, highlight = c(cv = -1, delta = 40)), "highlight")
})

test_that("plot draws the crosshair and colour key in all cases", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  tab <- sample_size_table()

  # Only the CV is on the grid (row 20), the delta is not: row band only
  expect_no_error(plot(tab, highlight = c(cv = 20, delta = 33)))
  # Only the delta is on the grid (column 40): column band only
  expect_no_error(plot(tab, highlight = c(cv = 22, delta = 40)))
  # Colour key with and without greyed-out cells, and with every cell greyed
  expect_no_error(plot(tab))
  expect_no_error(plot(tab, max_shown = 10))
  expect_no_error(plot(sample_size_table(cv = 50, delta = 10, max_n = 5)))
})

test_that("plot has no data citation, as the table uses no survey data", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_false("data_citation" %in% names(formals(plot.sample_size_table)))
  expect_no_error(plot(sample_size_table()))
})

test_that("plot looks the same in any window size, and leaves text size alone", {
  tab <- sample_size_table()
  for (size in list(c(14, 9), c(7, 7), c(5, 4))) {
    grDevices::pdf(NULL, width = size[1], height = size[2])
    expect_no_error(plot(tab, max_shown = 10, highlight = c(cv = 20, delta = 40)))
    # Text scaling is undone afterwards
    expect_equal(graphics::par("cex"), 1)
    grDevices::dev.off()
  }
})

test_that("plot redraws correctly when a viewer replays it at a new size", {
  # Plot viewers (VS Code, RStudio) record the plot and replay it when the
  # pane is resized. The drawing must run again, not reuse old positions.
  grDevices::pdf(NULL, width = 7, height = 7)
  grDevices::dev.control("enable")
  plot(sample_size_table(), max_shown = 10, highlight = c(cv = 20, delta = 40))
  recorded <- grDevices::recordPlot()
  grDevices::dev.off()

  grDevices::pdf(NULL, width = 12, height = 6)
  on.exit(grDevices::dev.off())
  expect_no_error(grDevices::replayPlot(recorded))
})
