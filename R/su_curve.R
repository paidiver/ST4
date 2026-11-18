#' Settings for sample unit size curves
#'
#' Holds the settings used by [su_curve()]. Every setting has a default, so
#' you only need to give the ones you want to change. Settings can be changed
#' in the same three ways as [sample_size_control()].
#'
#' @param times Number of random orders of tiles to draw. The curve is the
#'   median over all of them. 1000 follows the script used for Benoist et al.
#'   (2019).
#' @param replace Draw tiles with replacement, as Benoist et al. (2019) did.
#'   A tile can then be drawn more than once, and the curve can go past the
#'   number of tiles in the stratum. Without replacement each tile is used
#'   once, so every order ends with the same tiles and the spread shrinks to
#'   0 at the last tile. A warning is given when this is `FALSE`.
#' @param max_tiles Number of tiles to add up to. Leave it out to use the
#'   number of tiles in the stratum, or the largest of `steps` if given.
#' @param steps Numbers of tiles to record the curve at, for example
#'   `c(1, 2, 5, 10, 20, 50, 100)`. Leave it out to record every number of
#'   tiles up to `max_tiles`. Fewer steps run faster and give a smaller
#'   result.
#' @param band Share of the random orders inside the shaded band, centred on
#'   the median. 0.95 gives the band from the 2.5% to the 97.5% quantile.
#' @param ... Not used. A misspelt setting name ends up here and stops with
#'   an error that suggests the right name.
#'
#' @return A `su_curve_control` object: a list of every setting. The names of
#'   the settings you changed are kept in `attr(x, "changed")`.
#'
#' @references
#' Benoist, N.M.A., Morris, K.J., Bett, B.J., et al. (2019). Monitoring mosaic
#' biotopes in a marine conservation zone by autonomous underwater vehicle.
#' *Conservation Biology*, 33(5), 1174-1186. \doi{10.1111/cobi.13312}
#'
#' @examples
#' # All the defaults
#' su_curve_control()
#'
#' # Fewer orders, recorded at a few numbers of tiles
#' su_curve_control(times = 200, steps = c(1, 5, 10, 50, 100))
#' @export
su_curve_control <- function(times = 1000, replace = TRUE, max_tiles = NULL,
                             steps = NULL, band = 0.95, ...) {
  check_unknown_settings(list(...),
                         names(formals(su_curve_control)),
                         "su_curve_control")
  check_count(times, "times", min = 2)
  if (!is.logical(replace) || length(replace) != 1 || is.na(replace)) {
    stop("`replace` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(max_tiles)) check_count(max_tiles, "max_tiles", min = 1)
  if (!is.null(steps)) {
    if (!is.numeric(steps) || length(steps) == 0 || anyNA(steps) ||
        any(steps < 1) || any(steps != round(steps))) {
      stop("`steps` must be whole numbers of 1 or more.", call. = FALSE)
    }
    steps <- sort(unique(steps))
    if (!is.null(max_tiles) && any(steps > max_tiles)) {
      stop("`steps` cannot go past `max_tiles` (", max_tiles, ").",
           call. = FALSE)
    }
  }
  check_probability(band, "band")

  new_control(
    list(times = times, replace = replace, max_tiles = max_tiles,
         steps = steps, band = band),
    changed = setdiff(names(match.call())[-1], ""),
    class = "su_curve_control"
  )
}

#' Sample unit size curves
#'
#' Shows how each metric changes as more tiles are pooled into one sample
#' unit, to help choose the sample unit size. Tiles are drawn in a random
#' order and added one at a time. At each step the metric is worked out on
#' all the tiles drawn so far, pooled: density is all the individuals over
#' all the seabed area, and diversity indices are worked out by vegan on the
#' summed counts. This is repeated for many random orders, and the curve is
#' the median at each number of tiles, with a band showing the spread.
#'
#' This follows the approach of Benoist et al. (2019). By default tiles are
#' drawn with replacement, as in that paper (see [su_curve_control()]).
#'
#' The curves are worked out from running totals of the counts, so each
#' random order needs only one call to vegan for each metric, however many
#' tiles there are.
#'
#' @param x An `su_data` object of tiles, from [select_stratum()].
#' @param metrics Metrics to work out, as in [su_metrics()]. Defaults to
#'   richness.
#' @param control Settings from [su_curve_control()], or a list of them.
#' @param seed Seed for the random orders. The same seed gives the same
#'   curves. Setting it here does not change the random numbers used by the
#'   rest of your R session. If `NULL`, the session's random numbers are
#'   used, so [set.seed()] works as usual.
#' @param ... Settings to change, given straight to this function instead of
#'   through `control`. For example `times = 200`.
#'
#' @return A `su_curve` object: a data frame with one row per number of
#'   tiles and metric. The columns are `tiles`, `area` and `individuals`
#'   (medians over the random orders), `metric`, and the metric's `median`,
#'   `lower` and `upper` (edges of the band), `mean` and `sd`. The settings
#'   used are kept in `attr(x, "settings")`.
#'
#' @references
#' Benoist, N.M.A., Morris, K.J., Bett, B.J., et al. (2019). Monitoring mosaic
#' biotopes in a marine conservation zone by autonomous underwater vehicle.
#' *Conservation Biology*, 33(5), 1174-1186. \doi{10.1111/cobi.13312}
#'
#' @examples
#' counts_file <- system.file("extdata", "synthetic_counts.csv", package = "ST4")
#' metadata_file <- system.file("extdata", "synthetic_metadata.csv",
#'                              package = "ST4")
#' sand <- select_stratum(read_counts(counts_file, metadata_file), "sand")
#'
#' curves <- su_curve(sand, c("richness", "density"), times = 200, seed = 1)
#' curves
#' plot(curves)
#' @export
su_curve <- function(x, metrics = "richness", control = su_curve_control(),
                     seed = NULL, ...) {
  if (!inherits(x, "su_data")) {
    stop("`x` must be an su_data object from read_counts().", call. = FALSE)
  }
  if (!identical(x$settings$unit, "tiles")) {
    stop("`x` must hold tiles. It already holds ", x$settings$unit, ".",
         call. = FALSE)
  }
  control <- resolve_control(control, list(...), su_curve_control)
  metrics <- as_metric_list(metrics)
  n <- nrow(x$comm)

  # Fill in the number of tiles and the steps from the data
  max_tiles <- control$max_tiles
  if (is.null(max_tiles)) {
    max_tiles <- if (is.null(control$steps)) n else max(control$steps)
  }
  if (!control$replace && max_tiles > n) {
    stop("Without replacement the curve cannot go past the ", n, " tiles ",
         "in the data. Lower `max_tiles` or `steps`.", call. = FALSE)
  }
  steps <- if (is.null(control$steps)) seq_len(max_tiles) else control$steps
  control$max_tiles <- max_tiles
  control$steps <- steps

  if (is.null(x$settings$stratum)) {
    warning("No stratum selected, so the curves may mix strata. Use ",
            "select_stratum() first.", call. = FALSE)
  }
  if (!control$replace) {
    warning("Tiles are drawn without replacement, so every order ends with ",
            "the same tiles and the spread shrinks to 0 at the last tile.",
            call. = FALSE)
  }

  draw <- function() draw_curves(x, metrics, control$times, steps,
                                 max_tiles, control$replace)
  runs <- if (is.null(seed)) draw() else withr::with_seed(seed, draw())

  if (runs$n_empty > 0 && any(unlist(metrics) %in% diversity_metrics)) {
    warning("In ", runs$n_empty, " of ", control$times * length(steps),
            " steps the tiles drawn so far had no individuals. Diversity ",
            "indices there are as vegan gives them (Shannon 0, Simpson 1, ",
            "inverse Simpson Inf).", call. = FALSE)
  }

  probs <- c((1 - control$band) / 2, 1 - (1 - control$band) / 2)
  rows <- lapply(names(metrics), function(name) {
    v <- runs$values[, , name, drop = TRUE]
    v <- matrix(v, nrow = control$times)
    band <- apply(v, 2, stats::quantile, probs = probs, na.rm = TRUE,
                  names = FALSE)
    data.frame(
      tiles = steps,
      area = apply(runs$area, 2, stats::median),
      individuals = apply(runs$individuals, 2, stats::median),
      metric = name,
      median = apply(v, 2, stats::median, na.rm = TRUE),
      lower = band[1, ],
      upper = band[2, ],
      mean = colMeans(v, na.rm = TRUE),
      sd = apply(v, 2, stats::sd, na.rm = TRUE)
    )
  })

  structure(do.call(rbind, rows), settings = control,
            data = describe_data(x), stratum = describe_stratum(x),
            n_tiles = n,
            data_citation = x$settings$data_citation,
            class = c("su_curve", "data.frame"))
}

# Draws `times` random orders of tiles. For each order, takes running totals
# of the counts down the tiles, keeps the rows at `steps` and works out every
# metric on those rows in one go. Returns arrays of
# orders x steps (x metrics).
draw_curves <- function(x, metrics, times, steps, max_tiles, replace) {
  n <- nrow(x$comm)
  tile_area <- x$meta[[x$settings$area]]
  values <- array(NA_real_, dim = c(times, length(steps), length(metrics)),
                  dimnames = list(NULL, NULL, names(metrics)))
  area <- individuals <- matrix(NA_real_, times, length(steps))
  n_empty <- 0

  for (r in seq_len(times)) {
    order <- sample(n, max_tiles, replace = replace)
    pooled <- x$comm[order, , drop = FALSE]
    for (j in seq_len(ncol(pooled))) pooled[, j] <- cumsum(pooled[, j])
    pooled <- pooled[steps, , drop = FALSE]
    pooled_area <- cumsum(tile_area[order])[steps]

    area[r, ] <- pooled_area
    individuals[r, ] <- rowSums(pooled)
    n_empty <- n_empty + sum(individuals[r, ] == 0)
    for (name in names(metrics)) {
      m <- metrics[[name]]
      values[r, , name] <- if (is.function(m)) m(pooled) else
        builtin_metric(m, pooled, pooled_area)
    }
  }
  list(values = values, area = area, individuals = individuals,
       n_empty = n_empty)
}

#' @export
print.su_curve <- function(x, ...) {
  s <- attr(x, "settings")
  cat("Sample unit size curves\n")
  cat("Data: ", attr(x, "data"), "\n", sep = "")
  cat("Data citation: ", attr(x, "data_citation"), "\n", sep = "")
  cat(s$times, " random orders of tiles, ",
      if (s$replace) "with" else "without", " replacement. ",
      "Median and middle ", 100 * s$band, "% shown.\n", sep = "")

  # About 10 rows per metric, spread along the curve
  steps <- unique(x$tiles)
  shown <- steps[unique(round(seq(1, length(steps), length.out = 10)))]
  for (name in unique(x$metric)) {
    df <- as.data.frame(unclass(x[x$metric == name & x$tiles %in% shown, ]))
    df <- df[c("tiles", "area", "individuals", "median", "lower", "upper")]
    df[-1] <- lapply(df[-1], signif, 4)
    attr(df, "settings") <- attr(df, "data") <- NULL
    attr(df, "data_citation") <- attr(df, "stratum") <- NULL
    attr(df, "n_tiles") <- NULL
    cat("\n", name, "\n", sep = "")
    print(df, row.names = FALSE)
  }
  invisible(x)
}

#' Plot sample unit size curves
#'
#' Draws one panel per metric from [su_curve()]. The line is the median over
#' the random orders of tiles and the shaded band holds the middle of them
#' (95% by default). A panel on the right lists the settings used, and the
#' data citation is shown along the bottom.
#'
#' With `compare = TRUE` all the metrics are drawn in one panel instead, each
#' as a percentage of its value at the last step. This shows which metrics
#' settle quickly and which need more tiles.
#'
#' The sample unit size can be shown as seabed area, number of individuals or
#' number of tiles (images). Give one to use along the bottom, or two to add
#' the second along the top. The top axis is matched to the bottom one using
#' the median curves, so it is a guide.
#'
#' @param x A `su_curve` object from [su_curve()].
#' @param metrics Metrics to plot. Defaults to all of them.
#' @param x_axis Size measures for the bottom axis and, optionally, the top
#'   axis: `"area"`, `"individuals"` or `"tiles"`.
#' @param compare Draw all the metrics in one panel, each as a percentage of
#'   its value at the last step, to compare how quickly they settle.
#' @param main Plot title.
#' @param settings_panel Show the run settings panel on the right.
#' @param ... Other graphical arguments passed on to [graphics::lines()] for
#'   the median lines, such as `lwd`.
#'
#' @return `x`, invisibly.
#'
#' @examples
#' counts_file <- system.file("extdata", "synthetic_counts.csv", package = "ST4")
#' metadata_file <- system.file("extdata", "synthetic_metadata.csv",
#'                              package = "ST4")
#' sand <- select_stratum(read_counts(counts_file, metadata_file), "sand")
#' curves <- su_curve(sand, c("richness", "density"), times = 200, seed = 1)
#'
#' plot(curves)
#'
#' # Number of tiles along the bottom and seabed area along the top
#' plot(curves, x_axis = c("tiles", "area"))
#'
#' # Which metric settles first?
#' plot(curves, compare = TRUE, x_axis = "tiles")
#' @export
plot.su_curve <- function(x, metrics = NULL,
                          x_axis = c("area", "individuals"), compare = FALSE,
                          main = if (compare) "Comparing metrics" else
                            "Sample unit size curves",
                          settings_panel = TRUE, ...) {
  s <- attr(x, "settings")
  sizes <- c("area", "individuals", "tiles")
  if (!is.character(x_axis) || !length(x_axis) %in% 1:2 ||
      !all(x_axis %in% sizes) || anyDuplicated(x_axis)) {
    stop("`x_axis` must be one or two of \"area\", \"individuals\" and ",
         "\"tiles\".", call. = FALSE)
  }
  if (is.null(metrics)) metrics <- unique(x$metric)
  missing <- setdiff(metrics, x$metric)
  if (length(missing) > 0) {
    stop("Metrics not in the curves: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }

  curves <- lapply(metrics, function(name) {
    df <- as.data.frame(unclass(x[x$metric == name, ]))
    list(label = metric_label(name), size = df[sizes], median = df$median,
         lower = df$lower, upper = df$upper)
  })
  key <- NULL
  if (compare) {
    colours <- grDevices::hcl.colors(max(length(metrics), 2),
                                     "Dark 3")[seq_along(metrics)]
    panels <- list(list(curves = curves, colours = colours))
    key <- list(labels = metrics, colours = colours, type = "line")
  } else {
    panels <- curves
  }

  items <- NULL
  if (settings_panel) {
    items <- c(
      "Stratum" = attr(x, "stratum"),
      "Tiles in data" = attr(x, "n_tiles"),
      "Random orders" = s$times,
      "Replacement" = if (s$replace) "yes" else "no",
      "Tiles drawn" = paste("up to", s$max_tiles),
      "Line" = if (compare) "median, as % of its value at the last step" else
        "median"
    )
    if (!compare) {
      items <- c(items,
                 "Band" = paste0("middle ", 100 * s$band, "% of orders"))
    }
  }
  line_args <- list(...)
  draw_panel <- if (compare) {
    function(p, bottom, top) {
      draw_compare_panel(p, x_axis, line_args, bottom, top)
    }
  } else {
    function(p, bottom, top) {
      draw_su_curve_panel(p, x_axis, line_args, bottom, top)
    }
  }

  plot_figure(panels, draw_panel, main = main, items = items,
              footer = paste("Data:", attr(x, "data_citation")), key = key,
              top_axis = length(x_axis) == 2)
  invisible(x)
}

draw_su_curve_panel <- function(p, x_axis, line_args, bottom, top) {
  xv <- p$size[[x_axis[1]]]
  band_y <- c(p$lower, p$upper)
  ylim <- range(band_y[is.finite(band_y)], p$median[is.finite(p$median)])

  graphics::plot.new()
  graphics::plot.window(xlim = range(0, xv), ylim = ylim)
  panel_background(h = pretty(ylim, 3))

  ok <- is.finite(p$lower) & is.finite(p$upper)
  graphics::polygon(c(xv[ok], rev(xv[ok])), c(p$lower[ok], rev(p$upper[ok])),
                    col = grDevices::adjustcolor("#2C7FB8", alpha.f = 0.25),
                    border = NA)
  do.call(graphics::lines, c(list(xv, p$median, col = "#08306B", lwd = 2),
                             line_args))
  graphics::axis(2, at = pretty(ylim, 3), las = 1)
  graphics::box(col = "grey60")
  graphics::title(ylab = p$label, line = 3)
  draw_size_axes(p$size, x_axis, bottom, top)
}

# All the metrics in one panel, each as a % of its median at the last step
draw_compare_panel <- function(p, x_axis, line_args, bottom, top) {
  size <- p$curves[[1]]$size
  xv <- size[[x_axis[1]]]
  pct <- lapply(p$curves, function(curve) {
    last <- curve$median[length(curve$median)]
    if (is.finite(last) && last != 0) 100 * curve$median / last else
      rep(NA_real_, length(curve$median))
  })
  all_pct <- unlist(pct)
  ylim <- range(0, 100, all_pct[is.finite(all_pct)])

  graphics::plot.new()
  graphics::plot.window(xlim = range(0, xv), ylim = ylim)
  panel_background(h = pretty(ylim, 4))
  graphics::abline(h = 100, col = "grey60", lty = 2)
  for (i in seq_along(pct)) {
    do.call(graphics::lines, c(list(xv, pct[[i]], col = p$colours[i],
                                    lwd = 2), line_args))
  }
  graphics::axis(2, at = pretty(ylim, 4), las = 1)
  graphics::box(col = "grey60")
  graphics::title(ylab = "% of value at last step", line = 3)
  draw_size_axes(size, x_axis, bottom, top)
}

# The size axes below and above a curve panel. They are drawn into the
# strips around the panels, so need xpd = NA. The top axis is placed using
# the median curves.
draw_size_axes <- function(size, x_axis, bottom, top) {
  old <- graphics::par(xpd = NA)
  on.exit(graphics::par(old))
  if (bottom) {
    graphics::axis(1)
    graphics::mtext(size_label(x_axis[1]), side = 1, line = 2.2,
                    cex = graphics::par("cex"))
  }
  if (top) {
    top_size <- size[[x_axis[2]]]
    ticks <- pretty(range(top_size))
    at <- suppressWarnings(stats::approx(top_size, size[[x_axis[1]]],
                                         xout = ticks, ties = mean)$y)
    keep <- !is.na(at)
    graphics::axis(3, at = at[keep], labels = ticks[keep])
    graphics::mtext(size_label(x_axis[2]), side = 3, line = 2.2,
                    cex = graphics::par("cex"))
  }
}

size_label <- function(size) {
  switch(size,
    area = "Seabed area (m\u00b2)",
    individuals = "Individuals",
    tiles = "Tiles (images)"
  )
}

metric_label <- function(name) {
  labels <- c(richness = "Richness", abundance = "Individuals",
              density = "Density (/m\u00b2)", shannon = "Shannon H'",
              simpson = "Simpson 1 - D", invsimpson = "Inverse Simpson 1/D",
              pielou = "Pielou J'")
  if (name %in% names(labels)) labels[[name]] else name
}
