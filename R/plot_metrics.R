#' Plot the metrics for each unit
#'
#' Draws one histogram per metric from [su_metrics()], showing how the metric
#' is spread across the images or sample units. The mean is marked, and its
#' value and the coefficient of variation (CV) are written in each panel.
#' Values that are `NA`, `NaN` or `Inf` are left out, and the panel says how
#' many.
#'
#' @param x A `su_metrics` object from [su_metrics()].
#' @param metrics Metrics to plot. Defaults to all of them.
#' @param main Plot title.
#' @param settings_panel Show the run settings panel on the right.
#' @param ... Not used.
#'
#' @return `x`, invisibly.
#'
#' @examples
#' counts_file <- system.file("extdata", "synthetic_counts.csv", package = "ST4")
#' metadata_file <- system.file("extdata", "synthetic_metadata.csv",
#'                              package = "ST4")
#' hard <- select_stratum(read_counts(counts_file, metadata_file), "hard")
#' plot(su_metrics(hard, c("richness", "density", "shannon")))
#' @export
plot.su_metrics <- function(x, metrics = NULL,
                            main = "Metrics for each unit",
                            settings_panel = TRUE, ...) {
  check_metrics_shown(metrics, names(x)[-1])
  if (is.null(metrics)) metrics <- names(x)[-1]
  unit <- attr(x, "unit")

  panels <- lapply(metrics, function(name) {
    list(label = metric_label(name), values = x[[name]], unit = unit)
  })
  items <- NULL
  if (settings_panel) {
    items <- c("Stratum" = attr(x, "stratum"),
               "Units" = paste(nrow(x), unit),
               "Bars" = paste("number of", unit),
               "Orange line" = "mean of each metric")
  }
  plot_figure(panels, draw_histogram_panel, main = main, items = items,
              footer = paste("Data:", attr(x, "data_citation")),
              shared_x = FALSE, highlight = "Orange line")
  invisible(x)
}

draw_histogram_panel <- function(p, bottom, top) {
  v <- p$values
  ok <- is.finite(v)
  v <- v[ok]

  graphics::plot.new()
  if (length(v) == 0) {
    graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))
    graphics::text(0.5, 0.5, "No finite values")
    graphics::title(xlab = p$label, line = 2.2)
    return(invisible())
  }
  # Whole-number metrics such as richness get one bar per value
  breaks <- if (all(v == round(v)) && diff(range(v)) <= 50) {
    seq(min(v) - 0.5, max(v) + 0.5, by = 1)
  } else {
    "Sturges"
  }
  h <- graphics::hist(v, breaks = breaks, plot = FALSE)
  graphics::plot.window(xlim = range(h$breaks), ylim = c(0, max(h$counts)))
  panel_background(h = pretty(c(0, h$counts), 3))
  n <- length(h$counts)
  graphics::rect(h$breaks[-(n + 1)], 0, h$breaks[-1], h$counts,
                 col = grDevices::adjustcolor("#2C7FB8", alpha.f = 0.6),
                 border = "white")

  m <- mean(v)
  cv <- if (length(v) > 1 && m != 0) 100 * stats::sd(v) / m else NA
  graphics::abline(v = m, col = highlight_colour, lwd = 2)
  note <- paste0("mean ", signif(m, 3), ", CV ",
                 if (is.na(cv)) "NA" else round(cv, 1), "%")
  if (any(!ok)) note <- paste0(note, "\n", sum(!ok), " left out (not finite)")
  usr <- graphics::par("usr")
  graphics::text(usr[2], usr[4], note, adj = c(1.05, 1.3), cex = 0.75)

  graphics::axis(1)
  graphics::axis(2, at = pretty(c(0, h$counts), 3), las = 1)
  graphics::box(col = "grey60")
  graphics::title(xlab = p$label, ylab = tools_title(p$unit), line = 2.2)
}

#' Plot the coefficient of variation of each metric
#'
#' Draws a bar for the coefficient of variation (CV) of each metric from
#' [metric_cv()]. The metric with the highest CV is the worst case, and needs
#' the most sample units. Its bar is outlined.
#'
#' Give a second [metric_cv()] result as `y` to compare the two, for example
#' single images against composite sample units.
#'
#' @param x A `metric_cv` object from [metric_cv()].
#' @param y Optional second `metric_cv` object to compare with `x`. Only the
#'   metrics in `x` are shown.
#' @param main Plot title.
#' @param settings_panel Show the run settings panel on the right.
#' @param ... Not used.
#'
#' @return `x`, invisibly.
#'
#' @examples
#' counts_file <- system.file("extdata", "synthetic_counts.csv", package = "ST4")
#' metadata_file <- system.file("extdata", "synthetic_metadata.csv",
#'                              package = "ST4")
#' sand <- select_stratum(read_counts(counts_file, metadata_file), "sand")
#' images <- metric_cv(sand, c("richness", "density"))
#' plot(images)
#'
#' # Single images against composite sample units
#' sus <- metric_cv(build_sample_units(sand, seed = 1),
#'                  c("richness", "density"))
#' plot(images, sus)
#' @export
plot.metric_cv <- function(x, y = NULL, main = "Variation of each metric",
                           settings_panel = TRUE, ...) {
  series <- list(x)
  if (!is.null(y)) {
    if (!inherits(y, "metric_cv")) {
      stop("`y` must be a metric_cv object from metric_cv().", call. = FALSE)
    }
    series <- c(series, list(y))
  }
  values <- vapply(series, function(s) s$cv[match(x$metric, s$metric)],
                   numeric(nrow(x)))
  values <- matrix(values, nrow = nrow(x))
  colours <- c("#2C7FB8", "#9ECAE1")[seq_along(series)]
  worst <- vapply(series, function(s) worst_metric(s, x$metric), integer(1))

  key <- NULL
  items <- NULL
  if (settings_panel) {
    names_used <- vapply(series, function(s) describe_units(attr(s, "data")),
                         character(1))
    items <- c("Stratum" = describe_data_stratum(attr(x, "data")),
               "Data" = names_used[1])
    if (!is.null(y)) items <- c(items, "Compared with" = names_used[2])
    items <- c(items, "Worst case" = paste0(
      paste0(names_used, ": ", ifelse(is.na(worst), "none", x$metric[worst]),
             collapse = "\n"),
      "\nHighest CV, outlined in orange"
    ))
    key <- list(labels = names_used, colours = colours, type = "box")
  }
  panel <- list(values = values, labels = format_bar_values(values, "%"),
                metrics = vapply(x$metric, metric_label, character(1)),
                colours = colours, worst = worst,
                xlab = "CV (%)")
  plot_figure(list(panel), draw_bar_panel, main = main, items = items,
              footer = paste("Data:", attr(x, "data_citation")), key = key,
              mar_left = 6.5, highlight = "Worst case")
  invisible(x)
}

#' Plot the sample units needed for each metric
#'
#' Draws a bar for the number of sample units needed per stratum for each
#' metric and delta, from [sample_size_data()]. The metric with the highest
#' CV is the worst case, so the survey should be designed for it. Its bars
#' are outlined.
#'
#' @param x A `sample_size_data` object from [sample_size_data()].
#' @param main Plot title.
#' @param settings_panel Show the run settings panel on the right.
#' @param ... Not used.
#'
#' @return `x`, invisibly.
#'
#' @examples
#' counts_file <- system.file("extdata", "synthetic_counts.csv", package = "ST4")
#' metadata_file <- system.file("extdata", "synthetic_metadata.csv",
#'                              package = "ST4")
#' sand <- select_stratum(read_counts(counts_file, metadata_file), "sand")
#' sus <- build_sample_units(sand, seed = 1)
#' plot(sample_size_data(sus, delta = c(20, 40), metrics = c("richness",
#'                                                           "density")))
#' @export
plot.sample_size_data <- function(x,
                                  main = "Sample units needed per metric",
                                  settings_panel = TRUE, ...) {
  s <- attr(x, "settings")
  delta <- attr(x, "delta")
  values <- vapply(delta, function(d) as.numeric(x[[paste0("n_delta_", d)]]),
                   numeric(nrow(x)))
  values <- matrix(values, nrow = nrow(x))
  labels <- format_bar_values(values, "")
  # NA with a CV means more than max_n were needed
  too_many <- is.na(values) & !is.na(x$cv)
  labels[too_many] <- paste0(">", s$max_n)
  colours <- rev(grDevices::hcl.colors(length(delta) + 1,
                                       "Blues 3"))[-1]
  worst <- worst_metric(x, x$metric)

  key <- NULL
  items <- NULL
  if (settings_panel) {
    items <- c("Stratum" = describe_data_stratum(attr(x, "data")),
               "Data" = describe_units(attr(x, "data")),
               "Strata compared" = paste0(s$n_strata,
                 if (isTRUE(attr(x, "n_strata_from_data"))) " (from data)"),
               "Alpha" = s$alpha,
               "Power" = s$power,
               "Min n" = s$min_n,
               "Worst case" = if (is.na(worst)) "none" else
                 paste0(x$metric[worst], " (CV ", round(x$cv[worst], 1),
                        "%)\nHighest CV, outlined in orange"))
    key <- list(labels = paste0("delta ", delta, "%"), colours = colours,
                type = "box")
  }
  panel <- list(values = values, labels = labels,
                metrics = vapply(x$metric, metric_label, character(1)),
                colours = colours, worst = rep(worst, length(delta)),
                xlab = "Sample units needed per stratum")
  plot_figure(list(panel), draw_bar_panel, main = main, items = items,
              footer = paste("Data:", attr(x, "data_citation")), key = key,
              mar_left = 6.5, highlight = "Worst case")
  invisible(x)
}

# Horizontal bars, one group per metric from the top down and one bar per
# column of `values` in each group. `worst` gives, for each column, the row
# whose bar is outlined (NA for none). Its metric name is highlighted too.
draw_bar_panel <- function(p, bottom, top) {
  k <- nrow(p$values)
  n <- ncol(p$values)
  hi <- max(c(p$values[is.finite(p$values)], 1))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, hi * 1.15), ylim = c(0.5, k + 0.5),
                        xaxs = "i")
  panel_background(v = pretty(c(0, hi)))

  height <- 0.7 / n
  for (j in seq_len(n)) {
    y <- k:1 + 0.35 - height * (j - 0.5)
    v <- p$values[, j]
    ok <- is.finite(v)
    if (any(ok)) {
      graphics::rect(0, y[ok] - height / 2, v[ok], y[ok] + height / 2,
                     col = p$colours[j], border = NA)
    }
    w <- p$worst[j]
    if (!is.na(w) && ok[w]) {
      graphics::rect(0, y[w] - height / 2, v[w], y[w] + height / 2,
                     border = highlight_colour, lwd = 2)
    }
    graphics::text(ifelse(ok, v, 0), y, p$labels[, j], pos = 4, cex = 0.75,
                   offset = 0.3)
  }

  # Metric names, with every worst-case metric in bold orange
  hl <- unique(p$worst[!is.na(p$worst)])
  plain <- p$metrics
  plain[hl] <- ""
  graphics::axis(2, at = k:1, labels = plain, tick = FALSE, las = 1)
  if (length(hl) > 0) {
    graphics::axis(2, at = k - hl + 1, labels = p$metrics[hl], tick = FALSE,
                   las = 1, col.axis = highlight_colour, font = 2)
  }
  graphics::axis(1)
  graphics::box(col = "grey60")
  graphics::mtext(p$xlab, side = 1, line = 2.2, cex = graphics::par("cex"))
}

# Row of the metric with the highest CV in `s`, as a position in `metrics`
worst_metric <- function(s, metrics) {
  if (all(is.na(s$cv))) return(NA_integer_)
  match(s$metric[which.max(s$cv)], metrics)
}

format_bar_values <- function(values, suffix) {
  out <- ifelse(is.na(values), "NA", paste0(signif(values, 3), suffix))
  matrix(out, nrow = nrow(values))
}

# "160 images, stratum Substratum = sand" to "160 images"
describe_units <- function(data) {
  sub(",.*$", "", data)
}

# "160 images, stratum Substratum = sand" to "Substratum = sand"
describe_data_stratum <- function(data) {
  sub("^.*?, stratum ", "", data)
}

# "images" to "Images"
tools_title <- function(x) {
  paste0(toupper(substring(x, 1, 1)), substring(x, 2))
}

check_metrics_shown <- function(metrics, available) {
  missing <- setdiff(metrics, available)
  if (length(missing) > 0) {
    stop("Metrics not in the results: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
}
