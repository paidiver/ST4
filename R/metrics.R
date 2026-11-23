#' Metrics for each image or sample unit
#'
#' Calculates community metrics for each row of an `su_data` object. All the
#' diversity calculations are done by vegan. This function only picks the
#' right vegan function and puts the results in one table.
#'
#' Built-in metrics:
#'
#' | Name | What it is | Calculated by |
#' |---|---|---|
#' | `"richness"` | Number of taxa present | [vegan::specnumber()] |
#' | `"abundance"` | Number of individuals | Sum of counts |
#' | `"density"` | Individuals per m² | Abundance / seabed area |
#' | `"shannon"` | Shannon index H' | [vegan::diversity()] |
#' | `"simpson"` | Simpson index, 1 - D | [vegan::diversity()] |
#' | `"invsimpson"` | Inverse Simpson index, 1 / D | [vegan::diversity()] |
#' | `"pielou"` | Pielou evenness J', H' / ln(richness) | [vegan::diversity()] and [vegan::specnumber()] |
#'
#' Any other metric can be given as a named function that takes the counts
#' matrix (rows are units, columns are taxa) and returns one value per row.
#' For example `list(fisher = vegan::fisher.alpha)`.
#'
#' The values are returned as vegan gives them, with nothing changed. For a
#' unit with no individuals vegan gives Shannon 0, Simpson 1 and inverse
#' Simpson `Inf`, and Pielou is 0. A warning says how many units have no
#' individuals. Pielou is `NaN` for a unit with only one taxon, as it is
#' 0 / ln(1) = 0 / 0. Transform or filter the data first if these values are
#' not wanted.
#'
#' @param x An `su_data` object: single images from [read_counts()] or
#'   [select_stratum()], or composite SUs from [build_sample_units()].
#' @param metrics Names of the metrics to calculate (see the table above),
#'   and/or named functions. Defaults to richness.
#' @param ... Passed on to [vegan::diversity()], for example `base = 2` for
#'   Shannon in bits instead of the natural log.
#'
#' @return A `su_metrics` object: a data frame with one row per image or
#'   sample unit. The first column holds the ids and the others hold one
#'   metric each. Use [plot()][plot.su_metrics()] to see how each metric is
#'   spread across the units.
#'
#' @examples
#' counts_file <- system.file("extdata", "synthetic_counts.csv", package = "ST4")
#' metadata_file <- system.file("extdata", "synthetic_metadata.csv",
#'                              package = "ST4")
#' x <- read_counts(counts_file, metadata_file)
#'
#' # Richness only
#' head(su_metrics(x))
#'
#' # Several metrics
#' head(su_metrics(x, c("richness", "density", "shannon")))
#'
#' # Any other vegan metric, as a function
#' head(su_metrics(x, list("richness", fisher = vegan::fisher.alpha)))
#' @export
su_metrics <- function(x, metrics = "richness", ...) {
  if (!inherits(x, "su_data")) {
    stop("`x` must be an su_data object from read_counts().", call. = FALSE)
  }
  metrics <- as_metric_list(metrics)

  comm <- x$comm
  area <- x$meta[[x$settings$area]]
  empty <- rowSums(comm) == 0
  if (any(empty) && any(unlist(metrics) %in% diversity_metrics)) {
    warning(sum(empty), " of ", length(empty), " ", x$settings$unit,
            " have no individuals. Diversity indices for these are as vegan ",
            "gives them (Shannon 0, Simpson 1, inverse Simpson Inf).",
            call. = FALSE)
  }

  values <- lapply(names(metrics), function(name) {
    m <- metrics[[name]]
    if (is.function(m)) {
      out <- m(comm)
      if (length(out) != nrow(comm)) {
        stop("Metric `", name, "` must return one value per row.",
             call. = FALSE)
      }
      return(unname(out))
    }
    unname(builtin_metric(m, comm, area, ...))
  })
  names(values) <- names(metrics)

  out <- data.frame(id = rownames(comm), values, check.names = FALSE)
  names(out)[1] <- x$settings$id
  rownames(out) <- NULL
  structure(out, data = describe_data(x), stratum = describe_stratum(x),
            unit = x$settings$unit,
            data_citation = x$settings$data_citation,
            class = c("su_metrics", "data.frame"))
}

#' Summarise metrics for survey design
#'
#' Summarises the metrics from [su_metrics()] with the values that matter
#' for planning a survey, in place of the quartiles that [summary()] gives
#' for a data frame.
#'
#' For each metric it gives:
#' * `n_units`: units used. Units where the metric is `NA`, `NaN` or `Inf`
#'   are left out, as in [metric_cv()]. A CV from a few units is rougher
#'   than one from many.
#' * `mean`, `sd` and `cv` (%): as in [metric_cv()]. The CV is what
#'   [sample_size()] needs.
#' * `zeros`: units where the metric is 0. Many zeros, such as images with
#'   no individuals, mean the units are too small and the CV will be high.
#' * `left_out`: units left out because the metric is `NA`, `NaN` or `Inf`.
#'
#' The printout names the most variable metric (highest CV). It needs the
#' most SUs, so it sets the survey size if every metric is to be covered.
#'
#' @param object An `su_metrics` object from [su_metrics()]. A subset of its
#'   columns also works, such as `m[c("richness", "density")]`.
#' @param ... Not used.
#'
#' @return A `summary.su_metrics` object: a data frame with one row per
#'   metric and the columns `metric`, `n_units`, `mean`, `sd`, `cv`, `zeros`
#'   and `left_out`.
#'
#' @examples
#' counts_file <- system.file("extdata", "synthetic_counts.csv", package = "ST4")
#' metadata_file <- system.file("extdata", "synthetic_metadata.csv",
#'                              package = "ST4")
#' sand <- select_stratum(read_counts(counts_file, metadata_file), "sand")
#' m <- su_metrics(sand, c("richness", "density"))
#' summary(m)
#'
#' # The same for composite sample units
#' summary(su_metrics(build_sample_units(sand, seed = 1),
#'                    c("richness", "density")))
#' @export
summary.su_metrics <- function(object, ...) {
  # Metric columns are the numeric ones. The id column is text.
  values <- Filter(is.numeric, as.list(object))
  if (length(values) == 0) {
    stop("No metric columns to summarise.", call. = FALSE)
  }
  rows <- lapply(names(values), function(name) {
    v <- values[[name]]
    s <- cv_stats(v[is.finite(v)])
    data.frame(metric = name, n_units = s$n, mean = s$mean, sd = s$sd,
               cv = s$cv, zeros = sum(v == 0, na.rm = TRUE),
               left_out = sum(!is.finite(v)))
  })
  structure(do.call(rbind, rows), data = attr(object, "data"),
            data_citation = attr(object, "data_citation"),
            class = c("summary.su_metrics", "data.frame"))
}

#' @export
print.summary.su_metrics <- function(x, ...) {
  cat("Summary of metrics for survey design\n")
  if (!is.null(attr(x, "data"))) {
    cat("Data: ", attr(x, "data"), "\n", sep = "")
  }
  if (!is.null(attr(x, "data_citation"))) {
    cat("Data citation: ", attr(x, "data_citation"), "\n", sep = "")
  }
  cat("\n")
  df <- as.data.frame(unclass(x), stringsAsFactors = FALSE)
  attr(df, "data") <- NULL
  attr(df, "data_citation") <- NULL
  df$mean <- signif(df$mean, 4)
  df$sd <- signif(df$sd, 4)
  df$cv <- round(df$cv, 1)
  print(df, row.names = FALSE)
  cat("\n")
  if (all(is.na(x$cv))) {
    cat("No CV could be worked out.\n")
  } else {
    worst <- which.max(x$cv)
    cat("Most variable: ", x$metric[worst], " (CV ", round(x$cv[worst], 1),
        "%). It needs the most SUs.\n", sep = "")
  }
  if (any(x$left_out > 0)) {
    cat("Units where a metric is NA, NaN or Inf are left out for that",
        "metric (see left_out).\n")
  }
  invisible(x)
}

# Number of values, mean, standard deviation and CV (%) of `v`. The CV is NA
# when the mean is 0 or there are fewer than 2 values. Shared by
# metric_cv() and summary.su_metrics().
cv_stats <- function(v) {
  m <- if (length(v) > 0) mean(v) else NA_real_
  s <- if (length(v) > 1) stats::sd(v) else NA_real_
  cv <- if (!is.na(m) && !is.na(s) && m != 0) 100 * s / m else NA_real_
  list(n = length(v), mean = m, sd = s, cv = cv)
}

builtin_metric <- function(name, comm, area, ...) {
  switch(name,
    richness = vegan::specnumber(comm),
    abundance = rowSums(comm),
    density = rowSums(comm) / area,
    shannon = ,
    simpson = ,
    invsimpson = vegan::diversity(comm, index = name, ...),
    pielou = vegan::diversity(comm, index = "shannon", ...) /
      log(vegan::specnumber(comm))
  )
}

builtin_metrics <- c("richness", "abundance", "density", "shannon", "simpson",
                     "invsimpson", "pielou")
diversity_metrics <- c("shannon", "simpson", "invsimpson", "pielou")

# Turns the metrics argument into a named list of names and functions
as_metric_list <- function(metrics) {
  if (is.function(metrics)) {
    stop("Give functions a name, for example `list(fisher = my_function)`.",
         call. = FALSE)
  }
  metrics <- as.list(metrics)
  if (length(metrics) == 0) {
    stop("Give at least one metric.", call. = FALSE)
  }
  labels <- names(metrics)
  if (is.null(labels)) labels <- rep("", length(metrics))

  for (i in seq_along(metrics)) {
    m <- metrics[[i]]
    if (is.function(m)) {
      if (labels[i] == "") {
        stop("Give functions a name, for example `list(fisher = my_function)`.",
             call. = FALSE)
      }
    } else if (is.character(m) && length(m) == 1) {
      if (!m %in% builtin_metrics) {
        dist <- utils::adist(m, builtin_metrics, ignore.case = TRUE)[1, ]
        hint <- if (min(dist) <= 2) {
          paste0(" Did you mean `", builtin_metrics[which.min(dist)], "`?")
        } else {
          ""
        }
        stop("Unknown metric `", m, "`.", hint, "\nBuilt-in metrics: ",
             paste(builtin_metrics, collapse = ", "),
             ". Other metrics can be given as functions.", call. = FALSE)
      }
      if (labels[i] == "") labels[i] <- m
    } else {
      stop("Each metric must be a name such as \"richness\", or a function.",
           call. = FALSE)
    }
  }
  if (anyDuplicated(labels)) {
    stop("Metric names must be unique.", call. = FALSE)
  }
  stats::setNames(metrics, labels)
}

#' Coefficient of variation of each metric
#'
#' Calculates each metric for every image or sample unit with [su_metrics()],
#' then gives the mean, standard deviation and coefficient of variation (CV)
#' of each metric across the units. The CV is what [sample_size()] needs.
#'
#' CV (%) = 100 * standard deviation / mean, using the sample standard
#' deviation ([stats::sd()]). Units where a metric is `NA`, `NaN` or `Inf`
#' are left out for that metric, as no mean or CV can be worked out with
#' them. For example inverse Simpson is `Inf` for an image with no individuals.
#' The `n_units` column shows how many units were used for each metric, and
#' the printout says when any were left out. The CV is `NA` when the mean is
#' 0 or fewer than 2 units are left.
#'
#' The CV depends on the size of the units: it is usually much higher for
#' single images than for composite sample units made of many images.
#'
#' @inheritParams su_metrics
#'
#' @return A `metric_cv` object: a data frame with one row per metric and the
#'   columns `metric`, `n_units` (units used), `mean`, `sd` and `cv` (%). A
#'   description of the data (units, stratum) is kept in `attr(x, "data")`
#'   and shown when printed.
#'
#' @examples
#' counts_file <- system.file("extdata", "synthetic_counts.csv", package = "ST4")
#' metadata_file <- system.file("extdata", "synthetic_metadata.csv",
#'                              package = "ST4")
#' x <- select_stratum(read_counts(counts_file, metadata_file), "hard")
#' metric_cv(x, c("richness", "density", "shannon"))
#' @export
metric_cv <- function(x, metrics = "richness", ...) {
  values <- su_metrics(x, metrics, ...)[-1]

  rows <- lapply(names(values), function(name) {
    v <- values[[name]]
    s <- cv_stats(v[is.finite(v)])
    data.frame(metric = name, n_units = s$n, mean = s$mean, sd = s$sd,
               cv = s$cv)
  })
  structure(do.call(rbind, rows), data = describe_data(x),
            n_total = nrow(x$comm),
            data_citation = x$settings$data_citation,
            class = c("metric_cv", "data.frame"))
}

#' @export
print.metric_cv <- function(x, ...) {
  cat("Variation of each metric\n")
  cat("Data: ", attr(x, "data"), "\n", sep = "")
  cat("Data citation: ", attr(x, "data_citation"), "\n\n", sep = "")
  df <- as.data.frame(unclass(x), stringsAsFactors = FALSE)
  attr(df, "data") <- NULL
  attr(df, "data_citation") <- NULL
  attr(df, "n_total") <- NULL
  df$mean <- signif(df$mean, 4)
  df$sd <- signif(df$sd, 4)
  df$cv <- round(df$cv, 1)
  print(df, row.names = FALSE)
  left_out_note(x)
  invisible(x)
}

# Says when some units were left out of a metric's CV
left_out_note <- function(x) {
  if (any(x$n_units < attr(x, "n_total"))) {
    cat("Units where a metric is NA, NaN or Inf are left out for that",
        "metric (see n_units).\n")
  }
}
