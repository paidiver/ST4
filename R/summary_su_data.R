#' Summarise survey data before analysis
#'
#' A quick check of the data before working out metrics and sample sizes,
#' in place of the summary that [summary()] gives for a list. Works on
#' single images (`su_images`) and composite sample units (`su_composite`).
#'
#' It gives:
#' * Units with no individuals. Many empty units mean the units are too
#'   small, and diversity metrics and CVs will be unreliable.
#' * Individuals and seabed area per unit: mean and range. A wide range of
#'   areas means the units are uneven in size.
#' * Taxa seen, out of all the taxa in the data. [select_stratum()] keeps
#'   every taxon, so some may have no individuals in the stratum.
#' * Taxa seen once (singletons) or twice (doubletons). Many rare taxa mean
#'   richness keeps rising as more images are added.
#' * The most common taxa and their share of the individuals.
#' * For composite SUs: images per SU, and how many were left over and
#'   dropped.
#'
#' @param object An `su_data` object: single images from [read_counts()] or
#'   [select_stratum()], or composite SUs from [build_sample_units()].
#' @param top Number of most common taxa to show.
#' @param ... Not used.
#'
#' @return A `summary.su_data` object: a list of the values above, printed
#'   as a short report.
#'
#' @examples
#' counts_file <- system.file("extdata", "synthetic_counts.csv", package = "ST4")
#' metadata_file <- system.file("extdata", "synthetic_metadata.csv",
#'                              package = "ST4")
#' sand <- select_stratum(read_counts(counts_file, metadata_file), "sand")
#' summary(sand)
#'
#' # The same for composite sample units
#' summary(build_sample_units(sand, seed = 1))
#' @export
summary.su_data <- function(object, top = 5, ...) {
  check_count(top, "top", min = 1)
  comm <- object$comm
  s <- object$settings
  per_unit <- rowSums(comm)
  area <- object$meta[[s$area]]
  per_taxon <- colSums(comm)
  seen <- per_taxon[per_taxon > 0]
  common <- utils::head(sort(seen, decreasing = TRUE), top)

  out <- list(
    kind = class(object)[1],
    data = describe_data(object),
    data_citation = s$data_citation,
    unit = s$unit,
    n_units = nrow(comm),
    empty = sum(per_unit == 0),
    individuals = range_stats(per_unit),
    area = range_stats(area),
    n_taxa = ncol(comm),
    taxa_seen = length(seen),
    singletons = sum(seen == 1),
    doubletons = sum(seen == 2),
    common = data.frame(taxon = names(common),
                        individuals = unname(common),
                        share = 100 * unname(common) / sum(comm))
  )
  if (inherits(object, "su_composite")) {
    out$images_per_su <- range_stats(object$meta$n_images)
    out$images_used <- sum(object$meta$n_images)
    out$n_dropped <- s$n_dropped
  }
  structure(out, class = "summary.su_data")
}

#' @export
print.summary.su_data <- function(x, ...) {
  one <- if (x$kind == "su_composite") "SU" else "image"
  cat("Summary of the data\n")
  cat("Data: ", x$data, "\n", sep = "")
  cat("Data citation: ", x$data_citation, "\n\n", sep = "")

  cat("Units: ", x$n_units, " ", x$unit, "\n", sep = "")
  if (!is.null(x$images_per_su)) {
    cat("  Images per SU: ", format_range(x$images_per_su), "\n", sep = "")
    cat("  Images used: ", x$images_used, ", ", x$n_dropped,
        " left over and dropped\n", sep = "")
  }
  cat("  No individuals: ", x$empty, " (",
      round(100 * x$empty / x$n_units), "%)\n", sep = "")
  cat("  Individuals per ", one, ": ", format_range(x$individuals), "\n",
      sep = "")
  cat("  Seabed area per ", one, ": ",
      format_range(x$area, " m\u00b2"), "\n", sep = "")

  cat("Taxa: ", x$taxa_seen, " seen, of ", x$n_taxa, " in the data\n",
      sep = "")
  cat("  Seen once (singletons): ", x$singletons, "\n", sep = "")
  cat("  Seen twice (doubletons): ", x$doubletons, "\n", sep = "")

  if (nrow(x$common) > 0) {
    cat("\nMost common taxa\n")
    df <- x$common
    df$share <- paste0(round(df$share, 1), "%")
    print(df, row.names = FALSE)
  }
  if (x$empty > 0) {
    cat("\nDiversity indices for units with no individuals are as vegan",
        "gives them\n(Shannon 0, Simpson 1, inverse Simpson Inf).\n")
  }
  invisible(x)
}

# Mean, min and max of `v`
range_stats <- function(v) {
  c(mean = mean(v), min = min(v), max = max(v))
}

# "mean 7.3 m2, 5.9 to 8.8 m2", with an optional unit. Just the value when
# min and max are the same
format_range <- function(r, suffix = "") {
  f <- function(v) paste0(format(signif(v, 3)), suffix)
  if (r[["min"]] == r[["max"]]) return(f(r[["min"]]))
  paste0("mean ", f(r[["mean"]]), ", ", format(signif(r[["min"]], 3)), " to ",
         f(r[["max"]]))
}
