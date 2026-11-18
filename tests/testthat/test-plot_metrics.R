# Small made-up data where the right answer is known
plot_data <- function() {
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
  select_stratum(read_counts(counts, metadata, data_citation = "Test tiles"),
                 "sand")
}

# Runs a plot in a few window sizes and checks the text size is put back
expect_plots <- function(make_plot) {
  for (size in list(c(14, 9), c(9, 5.75), c(5, 4))) {
    grDevices::pdf(NULL, width = size[1], height = size[2])
    expect_no_error(make_plot())
    expect_equal(graphics::par("cex"), 1)
    grDevices::dev.off()
  }
}

test_that("su_metrics results keep a description of the data", {
  m <- su_metrics(plot_data(), c("richness", "abundance"))
  expect_s3_class(m, "su_metrics")
  expect_s3_class(m, "data.frame")
  expect_equal(attr(m, "data"), "6 tiles, stratum Substratum = sand")
  expect_equal(attr(m, "unit"), "tiles")
  expect_equal(attr(m, "data_citation"), "Test tiles")
})

test_that("plot of su_metrics works", {
  m <- suppressWarnings(su_metrics(plot_data(), c("richness", "density",
                                                  "shannon", "invsimpson")))
  expect_plots(function() plot(m))
  expect_plots(function() plot(m, metrics = "richness",
                               settings_panel = FALSE))
  expect_error(plot(m, metrics = "pielou"), "not in the results")
})

test_that("plot of metric_cv works alone and compared", {
  x <- plot_data()
  tiles <- metric_cv(x, c("richness", "abundance"))
  sus <- metric_cv(build_sample_units(x, type = "tiles", size = 2, seed = 1),
                   c("richness", "abundance"))
  expect_plots(function() plot(tiles))
  expect_plots(function() plot(tiles, sus))
  expect_error(plot(tiles, data.frame()), "metric_cv object")
})

test_that("plot of sample_size_data works, including more than max_n", {
  x <- plot_data()
  out <- sample_size_data(x, delta = c(20, 100),
                          metrics = c("richness", "abundance"), max_n = 10)
  expect_plots(function() plot(out))
})

test_that("compare plot of curves works", {
  curves <- suppressWarnings(
    su_curve(plot_data(), c("richness", "density", "shannon"), times = 20,
             seed = 1)
  )
  expect_plots(function() plot(curves, compare = TRUE))
  expect_plots(function() plot(curves, compare = TRUE, x_axis = "tiles"))
})

test_that("the worst case is the metric with the highest CV", {
  cv <- data.frame(metric = c("a", "b", "c"), cv = c(10, 30, NA))
  expect_equal(worst_metric(cv, c("a", "b", "c")), 2L)
  # Position is in the order of the metrics shown, not the table
  expect_equal(worst_metric(cv, c("c", "b", "a")), 2L)
  cv$cv <- NA
  expect_true(is.na(worst_metric(cv, c("a", "b", "c"))))
})

test_that("helpers split the data description", {
  data <- "160 tiles, stratum Substratum = sand"
  expect_equal(describe_units(data), "160 tiles")
  expect_equal(describe_data_stratum(data), "Substratum = sand")
  expect_equal(format_bar_values(matrix(c(12.34, NA)), "%"),
               matrix(c("12.3%", "NA")))
})

test_that("plots redraw correctly when a viewer replays them", {
  x <- plot_data()
  for (make_plot in list(
    function() plot(su_metrics(x, "richness")),
    function() plot(metric_cv(x, "richness"))
  )) {
    grDevices::pdf(NULL, width = 7, height = 7)
    grDevices::dev.control("enable")
    make_plot()
    recorded <- grDevices::recordPlot()
    grDevices::dev.off()
    grDevices::pdf(NULL, width = 12, height = 6)
    expect_no_error(grDevices::replayPlot(recorded))
    grDevices::dev.off()
  }
})
