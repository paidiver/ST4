# Input checks shared by the exported functions

check_positive <- function(x, name) {
  if (!is.numeric(x) || length(x) == 0 || anyNA(x) || any(x <= 0)) {
    stop("`", name, "` must be positive numbers.", call. = FALSE)
  }
}

check_probability <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x <= 0 || x >= 1) {
    stop("`", name, "` must be a single number between 0 and 1.", call. = FALSE)
  }
}

check_count <- function(x, name, min) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x < min || x != round(x)) {
    stop("`", name, "` must be a single whole number, at least ", min, ".",
         call. = FALSE)
  }
}
