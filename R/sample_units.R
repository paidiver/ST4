#' Settings for building sample units
#'
#' Holds the settings used by [build_sample_units()]. Every setting has a
#' default, so you only need to give the ones you want to change. Settings can
#' be changed in the same three ways as [sample_size_control()].
#'
#' There are three ways to group images into sample units (SUs):
#'
#' | `type` | Each SU holds | Default `size` |
#' |---|---|---|
#' | `"area"` | Images until the seabed area reaches `size` m² | 150 |
#' | `"individuals"` | Images until the number of individuals reaches `size` | 150 |
#' | `"images"` | A set number of images, `size` | 20 |
#'
#' The 150 m² and 150 individuals follow the composite sample units of
#' Benoist et al. (2019). The 20 images follow the script used for that
#' paper.
#'
#' @param type How to group the images: `"area"`, `"individuals"` or
#'   `"images"`.
#' @param size Target size of each SU, in the units of `type`. Leave it out
#'   to use the default for that type (see the table above).
#' @param ... Not used. A misspelt setting name ends up here and stops with
#'   an error that suggests the right name.
#'
#' @return A `sample_unit_control` object: a list of every setting. The names
#'   of the settings you changed are kept in `attr(x, "changed")`.
#'
#' @references
#' Benoist, N.M.A., Morris, K.J., Bett, B.J., et al. (2019). Monitoring mosaic
#' biotopes in a marine conservation zone by autonomous underwater vehicle.
#' *Conservation Biology*, 33(5), 1174-1186. \doi{10.1111/cobi.13312}
#'
#' @examples
#' # All the defaults
#' sample_unit_control()
#'
#' # SUs of 200 individuals each
#' sample_unit_control(type = "individuals", size = 200)
#' @export
sample_unit_control <- function(type = c("area", "individuals", "images"),
                                size = NULL, ...) {
  check_unknown_settings(list(...),
                         names(formals(sample_unit_control)),
                         "sample_unit_control")
  type <- match.arg(type)
  if (is.null(size)) {
    size <- c(area = 150, individuals = 150, images = 20)[[type]]
  }
  if (type == "area") {
    check_positive(size, "size")
    if (length(size) != 1) {
      stop("`size` must be a single number.", call. = FALSE)
    }
  } else {
    check_count(size, "size", min = 1)
  }

  new_control(
    list(type = type, size = size),
    changed = setdiff(names(match.call())[-1], ""),
    class = "sample_unit_control"
  )
}

#' Build composite sample units from images
#'
#' Puts the images in a random order and groups them into composite sample
#' units (SUs) of a set size. This is a posteriori stratified random sampling,
#' as in Benoist et al. (2019): SUs are built within one stratum, so use
#' [select_stratum()] first.
#'
#' Images are added to an SU in their random order until it reaches the target
#' size (see [sample_unit_control()]). The next image then starts a new SU. An
#' SU can go over the target by up to one image. Images left over at the end
#' that do not reach the target are dropped, so every SU is at least the
#' target size. The images are in a random order, so the images used are still
#' a random selection.
#'
#' The result is an `su_data` object with one row per SU, so [su_metrics()],
#' [metric_cv()] and [sample_size_data()] work on SUs the same way as on
#' images. Counts and seabed area are summed over the images in each SU.
#'
#' @param x An `su_images` object, from [select_stratum()]. Composite SUs
#'   cannot be used.
#' @param control Settings from [sample_unit_control()], or a list of them.
#' @param seed Seed for the random order. The same seed gives the same SUs.
#'   Setting it here does not change the random numbers used by the rest of
#'   your R session. If `NULL`, the session's random numbers are used, so
#'   [set.seed()] works as usual.
#' @param ... Settings to change, given straight to this function instead of
#'   through `control`. For example `type = "images"`.
#'
#' @return An `su_composite` object, with class
#'   `c("su_composite", "su_data")` and one row per SU. Its metadata has the
#'   columns `SU` (SU id), the seabed area and `n_images`. The images in each SU
#'   are listed in `x$members`, and the settings used and the number of images
#'   dropped are kept in `x$settings`.
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
#' # SUs of about 150 m² each
#' sus <- build_sample_units(sand, seed = 1)
#' sus
#' head(sus$members)
#'
#' # Metrics and CVs work on SUs the same way as on images
#' metric_cv(sus, c("richness", "density"))
#'
#' # SUs of 10 images each
#' build_sample_units(sand, type = "images", size = 10, seed = 1)
#' @export
build_sample_units <- function(x, control = sample_unit_control(),
                               seed = NULL, ...) {
  check_su_images(x)
  control <- resolve_control(control, list(...), sample_unit_control)
  if (is.null(x$settings$stratum)) {
    warning("No stratum selected, so SUs may mix strata. Use ",
            "select_stratum() first.", call. = FALSE)
  }

  order <- if (is.null(seed)) {
    sample(nrow(x$comm))
  } else {
    withr::with_seed(seed, sample(nrow(x$comm)))
  }

  area <- x$meta[[x$settings$area]]
  image_size <- switch(control$type,
    area = area,
    individuals = rowSums(x$comm),
    images = rep(1, nrow(x$comm))
  )
  su <- group_images(image_size[order], control$size)
  if (all(is.na(su))) {
    stop("Not enough images for one SU of ",
         format(control$size, scientific = FALSE), " ",
         su_size_unit(control$type), ". The data has ",
         format(round(sum(image_size), 1), scientific = FALSE), ".",
         call. = FALSE)
  }

  used <- order[!is.na(su)]
  su <- su[!is.na(su)]
  su_ids <- sprintf(paste0("SU_%0", nchar(max(su)), "d"), su)
  comm <- rowsum(x$comm[used, , drop = FALSE], su_ids, reorder = FALSE)
  meta <- data.frame(SU = rownames(comm),
                     area = as.vector(rowsum(area[used], su_ids,
                                             reorder = FALSE)),
                     n_images = as.vector(table(factor(su_ids,
                                                      rownames(comm)))))
  names(meta)[2] <- x$settings$area

  out <- new_su_data(comm, meta, id = "SU", area = x$settings$area,
                     data_citation = x$settings$data_citation,
                     kind = "su_composite")
  out$settings$by <- x$settings$by
  out$settings$stratum <- x$settings$stratum
  out$settings$sample_units <- control
  out$settings$n_dropped <- nrow(x$comm) - length(used)
  out$settings$seed <- seed
  out$members <- data.frame(SU = su_ids, image = rownames(x$comm)[used])
  names(out$members)[2] <- x$settings$id
  out
}

# Walks through the image sizes in order, adding each image to the current SU
# until it reaches `target`. Returns the SU number for each image, with NA for
# the leftover images at the end that do not reach the target.
group_images <- function(image_size, target) {
  su <- integer(length(image_size))
  current <- 1L
  total <- 0
  for (i in seq_along(image_size)) {
    su[i] <- current
    total <- total + image_size[i]
    if (total >= target) {
      current <- current + 1L
      total <- 0
    }
  }
  # The last SU never reached the target
  su[su == current] <- NA
  su
}

su_size_unit <- function(type) {
  switch(type, area = "m\u00b2", individuals = "individuals", images = "images")
}
