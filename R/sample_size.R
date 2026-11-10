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
#' until the inequality holds. When the power is 0.5 or more, the search
#' starts from the same formula with normal (z) values in place of t values.
#' Those t values are always larger than the z values, so no smaller n can
#' work and the result is the same as starting from n = 2, only faster.
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
#'   and `delta` (recycled to the same length). There is no upper limit by
#'   default. If `max_n` is set (see [sample_size_control()]), `NA` where more
#'   than `max_n` are needed.
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

  # Lower bound for n: Eqn 1 with z values. Above the median (probability 0.5)
  # t values are larger than z values, so for power >= 0.5 no smaller n can
  # work. Below that the bound does not hold, so start from 2.
  z <- stats::qnorm(1 - ctl$alpha / 2) + stats::qnorm(ctl$power)

  one <- function(cv, delta) {
    n <- if (ctl$power >= 0.5) max(2, floor(2 * (cv / delta)^2 * z^2)) else 2
    repeat {
      if (n > ctl$max_n) return(NA_integer_)
      v <- ctl$n_strata * (n - 1)
      t_alpha <- stats::qt(1 - ctl$alpha / 2, v) # two-tailed at alpha
      t_power <- stats::qt(ctl$power, v) # t_2(1-P)[v] is two-tailed at 2(1 - P)
      rhs <- 2 * (cv / delta)^2 * (t_alpha + t_power)^2
      if (n >= rhs) return(as.integer(n))
      n <- n + 1
    }
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
#'   Cells are `NA` only where `max_n` is set and more than that are needed. The settings used are
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
  cat("From Durden et al. (2016) Eqn 1 for set CVs, not from survey data\n")
  cat(s$n_strata, " strata compared, alpha ", s$alpha, ", power ", s$power,
      "\n", sep = "")
  cat("Rows are CV %. Columns are delta %.\n\n")

  m <- unclass(x)
  attr(m, "settings") <- NULL
  out <- ifelse(is.na(m), paste0(">", s$max_n), m) # keeps the dimnames
  print(out, quote = FALSE, right = TRUE)
  invisible(x)
}

#' Number of sample units required, from survey data
#'
#' Works out the coefficient of variation (CV) of each metric from pilot or
#' survey data with [metric_cv()], then the number of sample units needed per
#' stratum for each metric with [sample_size()].
#'
#' Different metrics vary by different amounts, so they need different
#' numbers of sample units. The survey should be designed for the metric
#' that needs the most, which is flagged as the worst case.
#'
#' The CV, and so the answer, depends on the size of the units in `x`. Use
#' data at the sample unit size you plan to survey with.
#'
#' @inheritParams sample_size
#' @inheritParams su_metrics
#' @param delta Smallest true differences to detect, in % of the mean. One
#'   column of results is given for each.
#' @param ... Settings to change, given straight to the function, such as
#'   `alpha = 0.1`. See [sample_size_control()].
#'
#' @return A `sample_size_data` object. This is the [metric_cv()] table with
#'   one extra column per `delta`, named like `n_delta_40`, holding the
#'   sample units required per stratum. `NA` where the CV could not be
#'   calculated, or where `max_n` is set and more than that are needed. The settings used are kept in
#'   `attr(x, "settings")` and the deltas in `attr(x, "delta")`.
#'
#' @examples
#' counts_file <- system.file("extdata", "synthetic_counts.csv", package = "ST4")
#' metadata_file <- system.file("extdata", "synthetic_metadata.csv",
#'                              package = "ST4")
#' x <- select_stratum(read_counts(counts_file, metadata_file), "hard")
#' sample_size_data(x, delta = c(40, 80),
#'                  metrics = c("richness", "density", "shannon"))
#' @export
sample_size_data <- function(x, delta, metrics = "richness",
                             control = sample_size_control(), ...) {
  check_positive(delta, "delta")
  ctl <- resolve_control(control, list(...), sample_size_control)
  out <- metric_cv(x, metrics)

  for (d in delta) {
    n <- rep(NA_integer_, nrow(out))
    ok <- !is.na(out$cv) & out$cv > 0
    if (any(ok)) n[ok] <- sample_size(out$cv[ok], d, control = ctl)
    out[[paste0("n_delta_", d)]] <- n
  }

  structure(out, settings = ctl, delta = delta, data = attr(out, "data"),
            data_citation = attr(out, "data_citation"),
            class = c("sample_size_data", "data.frame"))
}

#' @export
print.sample_size_data <- function(x, ...) {
  s <- attr(x, "settings")
  delta <- attr(x, "delta")
  cat("Sample units required per stratum, from survey data\n")
  cat("Data: ", attr(x, "data"), "\n", sep = "")
  cat("Data citation: ", attr(x, "data_citation"), "\n", sep = "")
  cat(s$n_strata, " strata compared, alpha ", s$alpha, ", power ", s$power,
      "\n\n", sep = "")

  df <- as.data.frame(unclass(x), stringsAsFactors = FALSE)
  attr(df, "settings") <- NULL
  attr(df, "delta") <- NULL
  attr(df, "data") <- NULL
  attr(df, "data_citation") <- NULL
  df$mean <- signif(df$mean, 4)
  df$sd <- signif(df$sd, 4)
  df$cv <- round(df$cv, 1)
  for (d in delta) {
    col <- paste0("n_delta_", d)
    df[[col]] <- ifelse(is.na(df[[col]]) & !is.na(df$cv),
                        paste0(">", s$max_n), df[[col]])
  }
  print(df, row.names = FALSE, na.print = "NA")

  # Worst case: the metric with the highest CV needs the most sample units
  if (any(!is.na(x$cv))) {
    worst <- which.max(x$cv)
    cat("\nWorst case: ", x$metric[worst], " (CV ", round(x$cv[worst], 1),
        "%). Design the survey for this metric.\n", sep = "")
  }
  invisible(x)
}
