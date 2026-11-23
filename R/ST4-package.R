#' @keywords internal
#' @section Terms:
#' * **Image**: a single photo of the seabed. The input data has one row of
#'   counts per image, with its seabed area. Data from photos stitched into a
#'   larger mosaic and cut into pieces ("tiles") is treated the same way, one
#'   row per piece.
#' * **Sample unit (SU)**: the replicate a metric is measured on and that the
#'   survey is designed around (n in Equation 1 of Durden et al. 2016, see
#'   [sample_size()]). There are two kinds:
#'   * **Single-image SU**: one image. [read_counts()] and [select_stratum()]
#'     give these, as an `su_images` object.
#'   * **Composite SU**: several images chosen at random from one stratum and
#'     pooled until they reach a target size (seabed area, individuals or
#'     number of images). [build_sample_units()] gives these, as an
#'     `su_composite` object.
#'
#' In one analysis each image is either its own SU or part of one composite
#' SU, never both. Both kinds are `su_data` objects, so [su_metrics()],
#' [metric_cv()] and [sample_size_data()] work on either. The number of
#' images needed for a survey is the number of SUs times the images per SU.
#'
#' @section AI assistance:
#' Large language models (LLMs) were used to help create this package. All
#' content has been reviewed by the authors.
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end

## mockable bindings: start
## mockable bindings: end
NULL
