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
#' @param n_strata Number of strata (groups) being compared, \eqn{a}.
#' @param alpha Significance level, two-tailed.
#' @param power Power of the test, \eqn{P}. Note Durden et al. (2016) also use
#'   P for significance in the text; here they are separate.
#' @param max_n Largest n to try. Returns `NA` if more are needed.
#'
#' @return Integer vector of SUs required per stratum, one per pair of `cv`
#'   and `delta` (recycled to the same length). `NA` where more than `max_n`
#'   are needed.
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
#' @export
sample_size <- function(cv, delta, n_strata = 2, alpha = 0.05, power = 0.9,
                        max_n = 100) {
  check_positive(cv, "cv")
  check_positive(delta, "delta")
  check_probability(alpha, "alpha")
  check_probability(power, "power")
  check_count(n_strata, "n_strata", min = 2)
  check_count(max_n, "max_n", min = 2)

  one <- function(cv, delta) {
    for (n in 2:max_n) {
      v <- n_strata * (n - 1)
      t_alpha <- stats::qt(1 - alpha / 2, v) # two-tailed at alpha
      t_power <- stats::qt(power, v)         # t_2(1-P)[v] is two-tailed at 2(1 - P)
      rhs <- 2 * (cv / delta)^2 * (t_alpha + t_power)^2
      if (n >= rhs) return(as.integer(n))
    }
    NA_integer_
  }

  unname(mapply(one, cv, delta))
}

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
