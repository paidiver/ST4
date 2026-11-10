#' Read counts and metadata for each tile
#'
#' Loads a counts table and a metadata table, joins them by tile id and checks
#' them. The result is an `su_data` object that the other functions in the
#' package take as input.
#'
#' The counts table has one row per tile and one column per taxon. Its first
#' column holds the tile ids. The metadata table has one row per tile, with a
#' column of tile ids, a column of seabed area and any other information such
#' as substratum or habitat.
#'
#' The tables are joined by tile id, never by row order, so the rows can be in
#' any order. Every tile must appear exactly once in both tables. Columns with
#' a blank name, such as the row numbers that [utils::write.csv()] adds, are
#' dropped from the metadata.
#'
#' @param counts Path to a CSV file of counts, or a data frame of counts.
#' @param metadata Path to a CSV file of metadata, or a data frame of
#'   metadata.
#' @param id Name of the tile id column in `metadata`.
#' @param area Name of the seabed area column in `metadata`, in m².
#' @param data_citation Free text describing or citing the data, such as
#'   `"Haig Fras AUV survey 2012, Benoist et al. (2019)"`. It is carried
#'   through every later step and shown in printouts and plots, so results
#'   always say where the data came from. It cannot be left empty.
#'
#' @return An `su_data` object. This is a list with:
#'   * `comm`: numeric matrix of counts with one row per tile and one column
#'     per taxon. Row names are the tile ids. This is the community matrix
#'     format that vegan uses.
#'   * `meta`: data frame of metadata, with rows in the same order as `comm`.
#'   * `settings`: the `id` and `area` column names.
#'
#' @examples
#' counts <- data.frame(
#'   tile = c("t1", "t2", "t3"),
#'   asterias = c(2, 0, 1),
#'   cerianthid = c(0, 3, 1)
#' )
#' metadata <- data.frame(
#'   FileName = c("t3", "t1", "t2"),
#'   Area_seabed_m2 = c(7.1, 7.4, 6.9),
#'   Substratum = c("sand", "hard", "sand")
#' )
#' x <- read_counts(counts, metadata)
#' x
#' x$comm
#'
#' # Say where the data came from
#' read_counts(counts, metadata, data_citation = "Made-up example tiles")
#' @export
read_counts <- function(counts, metadata, id = "FileName",
                        area = "Area_seabed_m2",
                        data_citation = "No Data Citation") {
  check_text(data_citation, "data_citation")
  counts <- read_table(counts, "counts")
  metadata <- read_table(metadata, "metadata")
  metadata <- metadata[!is.na(names(metadata)) & names(metadata) != ""]

  if (ncol(counts) < 2) {
    stop("`counts` needs a tile id column and at least one taxon column.",
         call. = FALSE)
  }
  for (col in c(id, area)) {
    if (!col %in% names(metadata)) {
      stop("Column `", col, "` not found in `metadata`.", call. = FALSE)
    }
  }

  count_ids <- as.character(counts[[1]])
  meta_ids <- as.character(metadata[[id]])
  check_ids(count_ids, meta_ids)

  comm <- as.matrix(counts[-1])
  if (!is.numeric(comm) || anyNA(comm) || any(comm < 0) ||
      any(comm != round(comm))) {
    stop("`counts` must hold whole numbers of 0 or more, with no missing ",
         "values.", call. = FALSE)
  }
  rownames(comm) <- count_ids

  # Put the metadata rows in the same order as the counts
  meta <- metadata[match(count_ids, meta_ids), , drop = FALSE]
  rownames(meta) <- NULL
  check_positive(meta[[area]], area)

  new_su_data(comm, meta, id = id, area = area,
              data_citation = trimws(data_citation))
}

#' @export
print.su_data <- function(x, ...) {
  s <- x$settings
  cat("su_data\n")
  cat("Data citation: ", s$data_citation, "\n", sep = "")
  cat("Stratum: ", describe_stratum(x), "\n", sep = "")
  cat("Units: ", nrow(x$comm), " ", s$unit, "\n", sep = "")
  cat("Taxa: ", ncol(x$comm), "\n", sep = "")
  cat("Individuals: ", sum(x$comm), "\n", sep = "")
  cat("Seabed area: ", format(sum(x$meta[[s$area]]), digits = 6), " m\u00b2\n",
      sep = "")
  other <- setdiff(names(x$meta), c(s$id, s$area))
  if (length(other) > 0) {
    cat("Other metadata: ", paste(other, collapse = ", "), "\n", sep = "")
  }
  invisible(x)
}

new_su_data <- function(comm, meta, id, area, data_citation,
                        unit = "tiles") {
  structure(
    list(comm = comm, meta = meta,
         settings = list(id = id, area = area, unit = unit,
                         data_citation = data_citation)),
    class = "su_data"
  )
}

# One-line descriptions of the data, for labelling printouts and plots
describe_stratum <- function(x) {
  s <- x$settings
  if (is.null(s$stratum)) "all (none selected)" else
    paste0(s$by, " = ", s$stratum)
}

describe_data <- function(x) {
  paste0(nrow(x$comm), " ", x$settings$unit, ", stratum ",
         describe_stratum(x))
}

read_table <- function(x, name) {
  if (is.data.frame(x)) {
    return(x)
  }
  if (is.character(x) && length(x) == 1) {
    if (!file.exists(x)) {
      stop("File for `", name, "` not found: ", x, call. = FALSE)
    }
    return(utils::read.csv(x, check.names = FALSE, stringsAsFactors = FALSE))
  }
  stop("`", name, "` must be a file path or a data frame.", call. = FALSE)
}

# Every tile must appear once in each table
check_ids <- function(count_ids, meta_ids) {
  for (side in list(list(count_ids, "counts"), list(meta_ids, "metadata"))) {
    dups <- unique(side[[1]][duplicated(side[[1]])])
    if (length(dups) > 0) {
      stop("Tile ids repeated in `", side[[2]], "`: ", show_some(dups),
           call. = FALSE)
    }
  }
  no_meta <- setdiff(count_ids, meta_ids)
  no_counts <- setdiff(meta_ids, count_ids)
  if (length(no_meta) > 0 || length(no_counts) > 0) {
    msg <- "Tile ids in `counts` and `metadata` do not match."
    if (length(no_meta) > 0) {
      msg <- paste0(msg, "\n", length(no_meta), " in `counts` only: ",
                    show_some(no_meta))
    }
    if (length(no_counts) > 0) {
      msg <- paste0(msg, "\n", length(no_counts), " in `metadata` only: ",
                    show_some(no_counts))
    }
    stop(msg, call. = FALSE)
  }
}

# The first few values, so error messages stay short
show_some <- function(x, n = 5) {
  out <- paste(utils::head(x, n), collapse = ", ")
  if (length(x) > n) out <- paste0(out, ", ...")
  out
}
