#' Select the tiles in one stratum
#'
#' Keeps only the tiles in one stratum, such as one substratum or one habitat.
#' Sample units are built and compared within a single stratum, so this is
#' usually the step after [read_counts()].
#'
#' All taxa are kept, including any with no individuals in the chosen stratum.
#' Taxa with zero counts do not change richness or the diversity indices.
#'
#' @param x An `su_data` object from [read_counts()].
#' @param stratum The stratum to keep, for example `"sand"`.
#' @param by Name of the metadata column that holds the strata.
#'
#' @return An `su_data` object holding only the tiles in `stratum`. The chosen
#'   column and stratum are kept in `x$settings$by` and `x$settings$stratum`.
#'
#' @examples
#' counts <- data.frame(
#'   tile = c("t1", "t2", "t3"),
#'   asterias = c(2, 0, 1),
#'   cerianthid = c(0, 3, 1)
#' )
#' metadata <- data.frame(
#'   FileName = c("t1", "t2", "t3"),
#'   Area_seabed_m2 = c(7.4, 6.9, 7.1),
#'   Substratum = c("hard", "sand", "sand")
#' )
#' x <- read_counts(counts, metadata)
#' select_stratum(x, "sand")
#' @export
select_stratum <- function(x, stratum, by = "Substratum") {
  if (!inherits(x, "su_data")) {
    stop("`x` must be an su_data object from read_counts().", call. = FALSE)
  }
  if (!is.character(by) || length(by) != 1 || !by %in% names(x$meta)) {
    stop("Column `", by, "` not found in the metadata.", call. = FALSE)
  }
  if (length(stratum) != 1 || is.na(stratum)) {
    stop("`stratum` must be a single value. One stratum at a time.",
         call. = FALSE)
  }

  values <- x$meta[[by]]
  keep <- !is.na(values) & values == stratum
  if (!any(keep)) {
    stop("No tiles with ", by, " = \"", stratum, "\". Strata in the data: ",
         show_some(sort(unique(values)), n = 20), call. = FALSE)
  }

  meta <- x$meta[keep, , drop = FALSE]
  rownames(meta) <- NULL
  out <- new_su_data(x$comm[keep, , drop = FALSE], meta,
                     id = x$settings$id, area = x$settings$area,
                     data_citation = x$settings$data_citation,
                     unit = x$settings$unit)
  out$settings$by <- by
  out$settings$stratum <- stratum
  out
}
