# Small made-up data where the right answer is known
curve_data <- function() {
  counts <- data.frame(
    tile = paste0("t", 1:6),
    a = c(2, 0, 1, 4, 0, 3),
    b = c(0, 3, 1, 0, 0, 2),
    c = c(0, 0, 0, 1, 0, 0)
  )
  metadata <- data.frame(
    FileName = paste0("t", 1:6),
    Area_seabed_m2 = c(5, 6, 5, 7, 5, 6),
    Substratum = "sand"
  )
  select_stratum(read_counts(counts, metadata), "sand")
}

# Three tiles that are all the same, so every random order gives the same
# curve
same_tiles <- function() {
  counts <- data.frame(tile = c("t1", "t2", "t3"), a = 2, b = 1)
  metadata <- data.frame(FileName = c("t1", "t2", "t3"), Area_seabed_m2 = 5,
                         Substratum = "sand")
  select_stratum(read_counts(counts, metadata), "sand")
}

test_that("gives the right answer when every order is the same", {
  out <- su_curve(same_tiles(), c("richness", "density", "shannon"),
                  times = 10, seed = 1)
  # Each tile has 2 of a and 1 of b on 5 m². After k tiles:
  #   area = 5k, individuals = 3k, richness = 2
  #   density = 3k / 5k = 0.6
  #   p_a = 2k / 3k = 2/3 and p_b = 1/3, so
  #   H' = -(2/3 * ln(2/3) + 1/3 * ln(1/3)) = 0.6365
  rich <- out[out$metric == "richness", ]
  expect_equal(rich$tiles, 1:3)
  expect_equal(rich$area, c(5, 10, 15))
  expect_equal(rich$individuals, c(3, 6, 9))
  expect_equal(rich$median, c(2, 2, 2))
  expect_equal(out$median[out$metric == "density"], rep(0.6, 3))
  h <- -(2 / 3 * log(2 / 3) + 1 / 3 * log(1 / 3))
  expect_equal(out$median[out$metric == "shannon"], rep(h, 3))
  # No spread, as every order is the same
  expect_equal(out$lower, out$upper)
  expect_equal(out$sd, rep(0, 9))
})

test_that("running totals give the same answer as re-summing at each step", {
  # The slow way, as in the script used for Benoist et al. (2019): for each
  # order, sum the first i tiles again at every step
  x <- curve_data()
  times <- 20
  out <- suppressWarnings(
    su_curve(x, c("richness", "shannon"), times = times, max_tiles = 10,
             seed = 3)
  )

  slow <- withr::with_seed(3, {
    rich <- shan <- matrix(NA_real_, times, 10)
    for (r in seq_len(times)) {
      order <- sample(6, 10, replace = TRUE)
      for (i in 1:10) {
        pooled <- colSums(x$comm[order[1:i], , drop = FALSE])
        rich[r, i] <- vegan::specnumber(pooled)
        shan[r, i] <- vegan::diversity(pooled, "shannon")
      }
    }
    list(rich = rich, shan = shan)
  })
  expect_equal(out$median[out$metric == "richness"],
               apply(slow$rich, 2, stats::median))
  expect_equal(out$mean[out$metric == "shannon"], colMeans(slow$shan))
  expect_equal(out$upper[out$metric == "shannon"],
               apply(slow$shan, 2, stats::quantile, probs = 0.975,
                     names = FALSE))
})

test_that("without replacement every order ends with all the tiles", {
  x <- curve_data()
  expect_warning(
    out <- su_curve(x, "richness", replace = FALSE, times = 20, seed = 1),
    "without replacement"
  )
  last <- out[out$tiles == 6, ]
  # All 6 tiles: taxa a, b and c are all present, so richness 3.
  # Area 5 + 6 + 5 + 7 + 5 + 6 = 34 m², with no spread.
  expect_equal(last$median, 3)
  expect_equal(last$area, 34)
  expect_equal(last$sd, 0)
  expect_error(su_curve(x, replace = FALSE, max_tiles = 7),
               "cannot go past the 6 tiles")
})

test_that("records only the steps asked for", {
  out <- su_curve(curve_data(), "richness", steps = c(1, 5, 20), times = 20,
                  seed = 1)
  expect_equal(out$tiles, c(1, 5, 20))
  # With replacement the curve can go past the 6 tiles in the data
  expect_equal(attr(out, "settings")$max_tiles, 20)
})

test_that("a narrower band sits inside a wider one", {
  x <- curve_data()
  wide <- su_curve(x, "richness", times = 200, seed = 1)
  narrow <- su_curve(x, "richness", times = 200, seed = 1, band = 0.5)
  expect_true(all(narrow$lower >= wide$lower))
  expect_true(all(narrow$upper <= wide$upper))
  expect_equal(narrow$median, wide$median)
})

test_that("the same seed gives the same curves and leaves the session alone", {
  x <- curve_data()
  set.seed(42)
  before <- .Random.seed
  a <- su_curve(x, times = 20, seed = 7)
  expect_equal(.Random.seed, before)
  expect_equal(su_curve(x, times = 20, seed = 7), a)
  set.seed(5)
  b <- su_curve(x, times = 20)
  set.seed(5)
  expect_equal(su_curve(x, times = 20), b)
})

test_that("takes metrics as named functions", {
  out <- su_curve(same_tiles(), list(total = rowSums), times = 5, seed = 1)
  # Pooled individuals after k tiles: 3k
  expect_equal(out$median, c(3, 6, 9))
})

test_that("warns when the tiles drawn so far have no individuals", {
  # Tile t5 has no individuals, so drawing it first gives an empty step
  expect_warning(su_curve(curve_data(), "shannon", steps = 1, times = 200,
                          seed = 1),
                 "had no individuals")
  # Richness of no individuals is 0, which needs no warning
  expect_no_warning(su_curve(curve_data(), "richness", steps = 1,
                             times = 200, seed = 1))
})

test_that("warns and stops where it should", {
  x <- curve_data()
  counts <- data.frame(tile = c("a", "b"), sp = c(1, 2))
  meta <- data.frame(FileName = c("a", "b"), Area_seabed_m2 = c(5, 5))
  expect_warning(su_curve(read_counts(counts, meta), times = 5, seed = 1),
                 "No stratum selected")
  expect_error(su_curve(x$comm), "su_data")
  sus <- build_sample_units(x, type = "tiles", size = 2, seed = 1)
  expect_error(su_curve(sus), "already holds SUs")
  expect_error(su_curve(x, tims = 5), "Did you mean `times`")
})

test_that("control checks its settings", {
  expect_equal(su_curve_control()$times, 1000)
  expect_true(su_curve_control()$replace)
  expect_equal(su_curve_control(steps = c(5, 1, 5))$steps, c(1, 5))
  expect_error(su_curve_control(steps = c(1, 50), max_tiles = 10),
               "cannot go past")
  expect_error(su_curve_control(replace = NA), "TRUE or FALSE")
  expect_error(su_curve_control(times = 1), "at least 2")
  expect_error(su_curve_control(band = 1), "between 0 and 1")
})

test_that("print shows the settings and a few rows per metric", {
  out <- su_curve(curve_data(), c("richness", "density"), times = 20,
                  seed = 1)
  expect_output(print(out), "20 random orders of tiles, with replacement")
  expect_output(print(out), "Median and middle 95% shown")
  expect_output(print(out), "density")
})

test_that("plot works in any window size and with each axis choice", {
  out <- suppressWarnings(
    su_curve(curve_data(), c("richness", "density", "shannon", "simpson"),
             times = 20, seed = 1)
  )
  for (size in list(c(14, 9), c(9, 5.75), c(5, 4))) {
    grDevices::pdf(NULL, width = size[1], height = size[2])
    expect_no_error(plot(out))
    expect_no_error(plot(out, metrics = "richness", x_axis = "tiles"))
    expect_no_error(plot(out, x_axis = c("tiles", "area"),
                         settings_panel = FALSE))
    expect_equal(graphics::par("cex"), 1)
    grDevices::dev.off()
  }
  expect_error(plot(out, x_axis = "volume"), "x_axis")
  expect_error(plot(out, metrics = "pielou"), "not in the curves")
})

test_that("plot redraws correctly when a viewer replays it at a new size", {
  out <- su_curve(curve_data(), "richness", times = 20, seed = 1)
  grDevices::pdf(NULL, width = 7, height = 7)
  grDevices::dev.control("enable")
  plot(out)
  recorded <- grDevices::recordPlot()
  grDevices::dev.off()

  grDevices::pdf(NULL, width = 12, height = 6)
  on.exit(grDevices::dev.off())
  expect_no_error(grDevices::replayPlot(recorded))
})
