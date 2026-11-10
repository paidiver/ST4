#' Number of sample units required
#'
#' Smallest number of sample units (SUs) per stratum needed to detect a given
#' difference between strata, from the coefficient of variation of a metric.
#' Uses Equation 1 of Durden et al. (2016), from Sokal & Rohlf (1995):
#'
#' \deqn{n \ge 2 \left(\frac{CV}{\delta}\right)^2 \left(t_{\alpha[v]} + t_{2(1-P)[v]}\right)^2}
#'
#' where \eqn{v = a(n - 1)} degrees of freedom, \eqn{a} is the number of
#' strata, \eqn{\alpha} is the significance level and \eqn{P} is the power of
#' the test. Both t values are two-tailed: \eqn{t_{\alpha[v]}} is
#' `qt(1 - alpha / 2, v)` and \eqn{t_{2(1-P)[v]}} is `qt(power, v)`.
#'
#' n appears on both sides (through \eqn{v}), so it is found by stepping n up
#' from 2 until the inequality holds.
#'
#' @param cv Coefficient of variation of the metric, in %.
#' @param delta Smallest true difference to detect, in % of the mean.
#' @param control Settings from [sample_size_control()]: the significance
#'   level \eqn{\alpha}, the power \eqn{P}, the number of strata \eqn{a} and
#'   the largest n to try. Can also be a list, such as `list(alpha = 0.1)`.
#'   Durden et al. (2016) use P for both power and significance in the text;
#'   here they are separate.
#' @param ... Settings to change, given straight to the function, such as
#'   `alpha = 0.1`. These override any in `control`.
#'
#' @return Integer vector of SUs required per stratum, one per pair of `cv`
#'   and `delta` (recycled to the same length). `NA` where more than `max_n`
#'   (see [sample_size_control()]) are needed.
#'
#' @references
#' Durden, J.M., Schoening, T., Althaus, F., et al. (2016). Perspectives in
#' visual imaging for marine biology and ecology: from acquisition to
#' understanding. *Oceanography and Marine Biology: An Annual Review*, 54,
#' 1-72. Equation 1 and Table 1.
#'
#' Sokal, R.R. & Rohlf, F.J. (1995). *Biometry: The Principles and Practice of
#' Statistics in Biological Research*, 3rd edn. New York: W.H. Freeman.
#'
#' @examples
#' # Detect a 40% difference between 2 strata when the CV is 20%
#' sample_size(cv = 20, delta = 40)
#'
#' # One CV with many differences
#' sample_size(cv = 30, delta = c(20, 40, 80))
#'
#' # Change a setting. These three lines give the same result.
#' sample_size(20, 40, control = sample_size_control(alpha = 0.1))
#' sample_size(20, 40, control = list(alpha = 0.1))
#' sample_size(20, 40, alpha = 0.1)
#' @export
sample_size <- function(cv, delta, control = sample_size_control(), ...) {
  check_positive(cv, "cv")
  check_positive(delta, "delta")
  ctl <- resolve_control(control, list(...), sample_size_control)

  one <- function(cv, delta) {
    for (n in 2:ctl$max_n) {
      v <- ctl$n_strata * (n - 1)
      t_alpha <- stats::qt(1 - ctl$alpha / 2, v) # two-tailed at alpha
      t_power <- stats::qt(ctl$power, v) # t_2(1-P)[v] is two-tailed at 2(1 - P)
      rhs <- 2 * (cv / delta)^2 * (t_alpha + t_power)^2
      if (n >= rhs) return(as.integer(n))
    }
    NA_integer_
  }

  unname(mapply(one, cv, delta))
}

#' Table of sample units required
#'
#' Runs [sample_size()] for every combination of `cv` and `delta`. The
#' defaults are the values used in Table 1 of Durden et al. (2016), so
#' `sample_size_table()` with no arguments recreates that table.
#'
#' @inheritParams sample_size
#' @param cv Coefficients of variation, in %. These become the table rows.
#' @param delta Smallest true differences to detect, in % of the mean. These
#'   become the table columns.
#'
#' @return A `sample_size_table` object. This is an integer matrix with one row
#'   per `cv` and one column per `delta`, holding the SUs required per stratum.
#'   Cells are `NA` where more than `max_n` are needed. The settings used are
#'   kept in `attr(x, "settings")` as a [sample_size_control()] object. Use
#'   `plot()` to draw it as a heatmap.
#'
#' @references
#' Durden, J.M., Schoening, T., Althaus, F., et al. (2016). Perspectives in
#' visual imaging for marine biology and ecology: from acquisition to
#' understanding. *Oceanography and Marine Biology: An Annual Review*, 54,
#' 1-72. Table 1.
#'
#' @seealso [sample_size()] for single values, [plot.sample_size_table()].
#'
#' @examples
#' # Recreate Durden et al. (2016) Table 1
#' tab <- sample_size_table()
#' tab
#' plot(tab, max_shown = 10)
#'
#' # A smaller table with 3 strata
#' sample_size_table(cv = c(10, 20, 30), delta = c(20, 40), n_strata = 3)
#' @export
sample_size_table <- function(cv = seq(5, 50, 5),
                              delta = c(10, 14, 20, 28, 40, 56, 79, 112, 158, 224),
                              control = sample_size_control(), ...) {
  ctl <- resolve_control(control, list(...), sample_size_control)
  grid <- expand.grid(cv = cv, delta = delta)
  n <- sample_size(grid$cv, grid$delta, control = ctl)

  structure(
    matrix(n, nrow = length(cv), dimnames = list(cv = cv, delta = delta)),
    settings = ctl,
    class = "sample_size_table"
  )
}

#' @export
print.sample_size_table <- function(x, ...) {
  s <- attr(x, "settings")
  cat("Sample units required per stratum\n")
  cat("Strata: ", s$n_strata, ", alpha: ", s$alpha, ", power: ", s$power,
      "\n", sep = "")
  cat("Rows are CV %. Columns are delta %.\n\n")

  m <- unclass(x)
  attr(m, "settings") <- NULL
  out <- ifelse(is.na(m), paste0(">", s$max_n), m) # keeps the dimnames
  print(out, quote = FALSE, right = TRUE)
  invisible(x)
}
