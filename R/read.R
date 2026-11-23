#' Read counts and metadata for each image
#'
#' Loads a counts table and a metadata table, joins them by image id and checks
#' them. The result is an `su_images` object (a kind of `su_data`) that the
#' other functions in the package take as input.
#'
#' The counts table has one row per image and one column per taxon. Its first
#' column holds the image ids. The metadata table has one row per image, with a
#' column of image ids, a column of seabed area and any other information such
#' as substratum or habitat.
#'
#' The tables are joined by image id, never by row order, so the rows can be in
#' any order. Every image must appear exactly once in both tables. Columns with
#' a blank name, such as the row numbers that [utils::write.csv()] adds, are
#' dropped from the metadata.
#'
#' @param counts Path to a CSV file of counts, or a data frame of counts.
#' @param metadata Path to a CSV file of metadata, or a data frame of
#'   metadata.
#' @param id Name of the image id column in `metadata`.
#' @param area Name of the seabed area column in `metadata`, in m².
#' @param data_citation Free text describing or citing the data, such as
#'   `"Haig Fras AUV survey 2012, Benoist et al. (2019)"`. It is carried
#'   through every later step and shown in printouts and plots, so results
#'   always say where the data came from. It cannot be left empty.
#'
#' @return An `su_images` object, with class `c("su_images", "su_data")`:
#'   each image is a single-image SU (see [ST4-package]). This is a list with:
#'   * `comm`: numeric matrix of counts with one row per image and one column
#'     per taxon. Row names are the image ids. This is the community matrix
#'     format that vegan uses.
#'   * `meta`: data frame of metadata, with rows in the same order as `comm`.
#'   * `settings`: the `id` and `area` column names.
#'
#' @examples
#' counts <- data.frame(
#'   image = c("t1", "t2", "t3"),
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
#' read_counts(counts, metadata, data_citation = "Made-up example images")
#' @export
read_counts <- function(counts, metadata, id = "FileName",
                        area = "Area_seabed_m2",
                        data_citation = "No Data Citation") {
  check_text(data_citation, "data_citation")
  counts <- read_table(counts, "counts")
  metadata <- read_table(metadata, "metadata")
  metadata <- metadata[!is.na(names(metadata)) & names(metadata) != ""]

  if (ncol(counts) < 2) {
    stop("`counts` needs an image id column and at least one taxon column.",
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
print.su_images <- function(x, ...) {
  print_su_data(x, c("Images" = nrow(x$comm)))
}

#' @export
print.su_composite <- function(x, ...) {
  s <- x$settings
  control <- s$sample_units
  per_su <- range(x$meta$n_images)
  print_su_data(x, c(
    "Composite SUs" = nrow(x$comm),
    "Built by" = paste0(control$type, ", at least ", format(control$size),
                        " ", su_size_unit(control$type), " each"),
    "Images used" = paste0(sum(x$meta$n_images), ", ", s$n_dropped,
                           " left over and dropped"),
    "Images per SU" = if (per_su[1] == per_su[2]) per_su[1] else
      paste(per_su, collapse = " to ")
  ))
}

# Printout shared by both kinds of su_data. `lines` are the lines that
# differ between them, shown after the stratum.
print_su_data <- function(x, lines) {
  s <- x$settings
  cat(class(x)[1], "\n", sep = "")
  cat("Data citation: ", s$data_citation, "\n", sep = "")
  cat("Stratum: ", describe_stratum(x), "\n", sep = "")
  cat(paste0(names(lines), ": ", lines, "\n"), sep = "")
  cat("Taxa: ", ncol(x$comm), "\n", sep = "")
  cat("Individuals: ", sum(x$comm), "\n", sep = "")
  cat("Seabed area: ", format(sum(x$meta[[s$area]]), digits = 6), " m\u00b2\n",
      sep = "")
  other <- setdiff(names(x$meta), c(s$id, s$area, "n_images"))
  if (length(other) > 0) {
    cat("Other metadata: ", paste(other, collapse = ", "), "\n", sep = "")
  }
  invisible(x)
}

# Makes an su_data object. `kind` is "su_images" for single-image SUs (from
# read_counts() and select_stratum()) or "su_composite" for composite SUs
# (from build_sample_units()). The unit label for printouts and plots goes
# in `settings$unit`.
new_su_data <- function(comm, meta, id, area, data_citation,
                        kind = c("su_images", "su_composite")) {
  kind <- match.arg(kind)
  unit <- if (kind == "su_images") "images" else "composite SUs"
  structure(
    list(comm = comm, meta = meta,
         settings = list(id = id, area = area, unit = unit,
                         data_citation = data_citation)),
    class = c(kind, "su_data")
  )
}

# Stops unless `x` holds single images. build_sample_units() and su_curve()
# pool images, so they cannot take composite SUs.
check_su_images <- function(x) {
  if (!inherits(x, "su_data")) {
    stop("`x` must be an su_data object from read_counts().", call. = FALSE)
  }
  if (!inherits(x, "su_images")) {
    stop("`x` must hold single images (su_images), from read_counts() or ",
         "select_stratum(). It holds ", x$settings$unit, " (",
         class(x)[1], ").", call. = FALSE)
  }
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

# Every image must appear once in each table
check_ids <- function(count_ids, meta_ids) {
  for (side in list(list(count_ids, "counts"), list(meta_ids, "metadata"))) {
    dups <- unique(side[[1]][duplicated(side[[1]])])
    if (length(dups) > 0) {
      stop("Image ids repeated in `", side[[2]], "`: ", show_some(dups),
           call. = FALSE)
    }
  }
  no_meta <- setdiff(count_ids, meta_ids)
  no_counts <- setdiff(meta_ids, count_ids)
  if (length(no_meta) > 0 || length(no_counts) > 0) {
    msg <- "Image ids in `counts` and `metadata` do not match."
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
