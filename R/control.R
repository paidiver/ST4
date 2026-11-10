#' Settings for the number of sample units
#'
#' Holds the settings used by [sample_size()] and [sample_size_table()]. Every
#' setting has a default, so you only need to give the ones you want to
#' change. This follows the same pattern as [stats::glm.control()].
#'
#' There are three ways to change a setting, which all give the same result:
#'
#' ```
#' sample_size(20, 40, control = sample_size_control(alpha = 0.1))
#' sample_size(20, 40, control = list(alpha = 0.1))
#' sample_size(20, 40, alpha = 0.1)
#' ```
#'
#' Run `sample_size_control()` on its own to see every setting and its
#' default.
#'
#' @param alpha Significance level, two-tailed. Between 0 and 1.
#' @param power Power of the test, P. Between 0 and 1.
#' @param n_strata Number of strata (groups) being compared. At least 2.
#' @param max_n Largest number of sample units to try. Cells needing more are
#'   returned as `NA`.
#' @param ... Not used. A misspelt setting name ends up here and stops with
#'   an error that suggests the right name.
#'
#' @return A `sample_size_control` object: a list of every setting. The
#'   names of the settings you changed are kept in `attr(x, "changed")`.
#'
#' @examples
#' # All the defaults
#' sample_size_control()
#'
#' # Change one setting
#' sample_size_control(alpha = 0.1)
#' @export
sample_size_control <- function(alpha = 0.05, power = 0.9, n_strata = 2,
                                max_n = 100, ...) {
  check_unknown_settings(list(...),
                         names(formals(sample_size_control)),
                         "sample_size_control")
  check_probability(alpha, "alpha")
  check_probability(power, "power")
  check_count(n_strata, "n_strata", min = 2)
  check_count(max_n, "max_n", min = 2)

  new_control(
    list(alpha = alpha, power = power, n_strata = n_strata, max_n = max_n),
    changed = setdiff(names(match.call())[-1], ""),
    class = "sample_size_control"
  )
}

#' @export
print.st4_control <- function(x, ...) {
  changed <- attr(x, "changed")
  cat(class(x)[1], "\n", sep = "")
  values <- vapply(x, function(v) paste(format(v), collapse = ", "),
                   character(1))
  marks <- ifelse(names(x) %in% changed, "  (changed)", "")
  lines <- paste0("  ", format(names(x)), "  ", format(values), marks)
  cat(lines, sep = "\n")
  invisible(x)
}

new_control <- function(values, changed, class) {
  structure(values, changed = changed, class = c(class, "st4_control"))
}

# Turns the control argument and any settings given straight to a function
# into one control object. `control` can be a control object or a plain list.
# Settings in `dots` win over those in `control`.
resolve_control <- function(control, dots, make) {
  if (inherits(control, "st4_control")) {
    control <- unclass(control)[attr(control, "changed")]
  } else if (!is.list(control)) {
    stop("`control` must be made by a *_control() function, or be a list.",
         call. = FALSE)
  }
  if (length(dots) > 0 &&
      (is.null(names(dots)) || any(names(dots) == ""))) {
    stop("Settings must be named, for example `alpha = 0.1`.", call. = FALSE)
  }
  do.call(make, utils::modifyList(control, dots))
}

check_unknown_settings <- function(dots, valid, fun) {
  if (length(dots) == 0) return(invisible())
  valid <- setdiff(valid, "...")
  given <- names(dots)
  if (is.null(given) || any(given == "")) {
    stop("Settings must be named, for example `alpha = 0.1`.", call. = FALSE)
  }
  msgs <- vapply(given, function(name) {
    dist <- utils::adist(name, valid, ignore.case = TRUE)[1, ]
    hint <- if (min(dist) <= 2) {
      paste0(" Did you mean `", valid[which.min(dist)], "`?")
    } else {
      ""
    }
    paste0("Unknown setting `", name, "`.", hint)
  }, character(1))
  stop(paste(msgs, collapse = "\n"), "\nSettings for ", fun, "(): ",
       paste(valid, collapse = ", "), call. = FALSE)
}
