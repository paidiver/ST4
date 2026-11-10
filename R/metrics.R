#' Metrics for each tile or sample unit
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
#' Shannon, Simpson, inverse Simpson and Pielou are `NA` for units with no
#' individuals, as they are not defined there. Pielou is also `NA` for units
#' with only one taxon.
#'
#' @param x An `su_data` object from [read_counts()] or [select_stratum()].
#' @param metrics Names of the metrics to calculate (see the table above),
#'   and/or named functions. Defaults to richness.
#' @param ... Passed on to [vegan::diversity()], for example `base = 2` for
#'   Shannon in bits instead of the natural log.
#'
#' @return A data frame with one row per tile or sample unit. The first column
#'   holds the ids and the others hold one metric each.
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
    unname(builtin_metric(m, comm, area, empty, ...))
  })
  names(values) <- names(metrics)

  out <- data.frame(id = rownames(comm), values, check.names = FALSE)
  names(out)[1] <- x$settings$id
  rownames(out) <- NULL
  out
}

builtin_metric <- function(name, comm, area, empty, ...) {
  switch(name,
    richness = vegan::specnumber(comm),
    abundance = rowSums(comm),
    density = rowSums(comm) / area,
    shannon = ,
    simpson = ,
    invsimpson = {
      out <- vegan::diversity(comm, index = name, ...)
      out[empty] <- NA
      out
    },
    pielou = {
      out <- vegan::diversity(comm, index = "shannon", ...) /
        log(vegan::specnumber(comm))
      out[empty | vegan::specnumber(comm) < 2] <- NA
      out
    }
  )
}

builtin_metrics <- c("richness", "abundance", "density", "shannon", "simpson",
                     "invsimpson", "pielou")

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
#' Calculates each metric for every tile or sample unit with [su_metrics()],
#' then gives the mean, standard deviation and coefficient of variation (CV)
#' of each metric across the units. The CV is what [sample_size()] needs.
#'
#' CV (%) = 100 * standard deviation / mean, using the sample standard
#' deviation ([stats::sd()]). Units where a metric is `NA`, such as Shannon on
#' a tile with no individuals, are left out for that metric. The CV is `NA`
#' when the mean is 0 or fewer than 2 units are left.
#'
#' The CV depends on the size of the units: it is usually much higher for
#' single tiles than for composite sample units made of many tiles.
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
    v <- v[!is.na(v)]
    m <- if (length(v) > 0) mean(v) else NA_real_
    s <- if (length(v) > 1) stats::sd(v) else NA_real_
    cv <- if (!is.na(m) && !is.na(s) && m != 0) 100 * s / m else NA_real_
    data.frame(metric = name, n_units = length(v), mean = m, sd = s, cv = cv)
  })
  structure(do.call(rbind, rows), data = describe_data(x),
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
  df$mean <- signif(df$mean, 4)
  df$sd <- signif(df$sd, 4)
  df$cv <- round(df$cv, 1)
  print(df, row.names = FALSE)
  invisible(x)
}
