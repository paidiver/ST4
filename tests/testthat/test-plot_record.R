# Plot viewers (VS Code, RStudio) redraw a plot at a new size by replaying
# R's record of it. Each plot must start a fresh record, or the viewer draws
# every earlier plot underneath it.

record_data <- function() {
  counts <- data.frame(image = paste0("t", 1:6), a = c(2, 0, 1, 4, 0, 3),
                       b = c(0, 3, 1, 0, 0, 2))
  metadata <- data.frame(FileName = paste0("t", 1:6),
                         Area_seabed_m2 = c(5, 6, 5, 7, 5, 6),
                         Substratum = "sand")
  select_stratum(read_counts(counts, metadata), "sand")
}

all_plots <- function() {
  x <- record_data()
  curves <- su_curve(x, c("richness", "abundance"), times = 10, seed = 1)
  list(
    heatmap = function() plot(sample_size_table(), highlight = c(cv = 20,
                                                                 delta = 40)),
    curves = function() plot(curves),
    compare = function() plot(curves, compare = TRUE),
    metrics = function() plot(su_metrics(x, c("richness", "abundance"))),
    cv = function() plot(metric_cv(x, "richness")),
    sample_size = function() plot(sample_size_data(x, delta = 50))
  )
}

test_that("each plot starts a fresh record, so viewers do not stack plots", {
  plots <- all_plots()
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  grDevices::dev.control("enable")
  for (name in names(plots)) {
    plots[[name]]()
    first <- length(grDevices::recordPlot()[[1]])
    plots[[name]]()
    # The same plot again gives a record of the same length, not one that
    # also holds the first plot
    expect_equal(length(grDevices::recordPlot()[[1]]), first, label = name)
  }
})

test_that("each plot is on its own page", {
  plots <- all_plots()
  file <- withr::local_tempfile(fileext = ".pdf")
  grDevices::pdf(file, compress = FALSE)
  for (p in plots) p()
  graphics::plot(1:3) # an ordinary plot afterwards gets its own page too
  grDevices::dev.off()
  pdf_text <- readLines(file, warn = FALSE)
  n_pages <- sum(grepl("/Type /Page$|/Type /Page ", pdf_text, useBytes = TRUE))
  expect_equal(n_pages, length(plots) + 1)
})
