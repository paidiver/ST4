#' Plot a table of sample units required
#'
#' Draws a [sample_size_table()] as a heatmap with CV down the side and delta
#' along the top, laid out like Table 1 of Durden et al. (2016). Each cell
#' shows the number of SUs required per stratum. Darker cells need more SUs.
#'
#' @param x A `sample_size_table` object from [sample_size_table()].
#' @param max_shown Cells needing more than this are greyed out instead of
#'   coloured. Durden et al. (2016) grey out cells above 10. By default no
#'   cells are greyed out, unless `max_n` is set and cells need more.
#' @param main Plot title.
#' @param ... Other graphical arguments passed on to [graphics::image()].
#'
#' @return `x`, invisibly.
#'
#' @examples
#' plot(sample_size_table(), max_shown = 10)
#' @export
plot.sample_size_table <- function(x, max_shown = NULL,
                                   main = "Sample units required per stratum",
                                   ...) {
  s <- attr(x, "settings")
  m <- unclass(x)
  attr(m, "settings") <- NULL
  cv <- rownames(m)
  delta <- colnames(m)

  greyed <- is.na(m)
  if (!is.null(max_shown)) {
    check_count(max_shown, "max_shown", min = 1)
    greyed <- greyed | m > max_shown
  }
  shown <- m
  shown[greyed] <- NA

  # image() puts the first row at the bottom, so flip the rows to read like the
  # paper with the smallest CV at the top
  z <- t(shown[rev(seq_along(cv)), , drop = FALSE])
  lo <- if (all(greyed)) 0 else min(shown, na.rm = TRUE)
  hi <- if (all(greyed)) 0 else max(shown, na.rm = TRUE)
  colours <- grDevices::hcl.colors(hi - lo + 1, "Blues 3", rev = TRUE)

  old <- graphics::par(mar = c(1, 4.5, 6, 1))
  on.exit(graphics::par(old))

  # Grey background first, so greyed (NA) cells show through the heatmap
  graphics::plot.new()
  graphics::plot.window(xlim = c(0.5, length(delta) + 0.5),
                        ylim = c(0.5, length(cv) + 0.5), xaxs = "i", yaxs = "i")
  graphics::rect(0.5, 0.5, length(delta) + 0.5, length(cv) + 0.5,
                 col = "grey85", border = NA)
  if (!all(greyed)) {
    graphics::image(seq_along(delta), seq_along(cv), z, col = colours,
                    zlim = c(lo, hi), add = TRUE, ...)
  }
  graphics::title(ylab = "CV (%)")

  graphics::axis(3, at = seq_along(delta), labels = delta, tick = FALSE)
  graphics::axis(2, at = seq_along(cv), labels = rev(cv), las = 1, tick = FALSE)
  graphics::mtext("Delta (%)", side = 3, line = 2.2)
  graphics::title(main, line = 4.2)
  graphics::mtext(paste0("Strata: ", s$n_strata, ", alpha: ", s$alpha,
                         ", power: ", s$power),
                  side = 3, line = 3.3, cex = 0.8)

  # Label each cell, with white text on the darker half of the colours
  labels <- t(m[rev(seq_along(cv)), , drop = FALSE])
  labels <- ifelse(is.na(labels), paste0(">", s$max_n), labels)
  level <- (z - lo) / max(1, hi - lo)
  text_col <- ifelse(!is.na(level) & level > 0.5, "white", "black")
  graphics::text(rep(seq_along(delta), length(cv)),
                 rep(seq_along(cv), each = length(delta)),
                 labels, col = text_col, cex = 0.8)

  invisible(x)
}
