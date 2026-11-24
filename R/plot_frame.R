# Shared layout for the figures with one or more panels: a title in a rounded
# box at the top, the panels in one or two columns inside a second rounded
# box, a run settings panel down the right and a footer along the bottom.
# Called by plot_figure(), which starts the page and records the drawing so
# it is worked out again whenever a plot viewer redraws it at a new size.
#
# `draw_panel(panel, bottom, top)` draws one panel. `bottom` and `top` say
# whether it should draw the x axis below it and the second x axis above it.
# With `shared_x = TRUE` the panels share the x axis, so it is drawn only
# under the lowest panel of each column, in a strip below the panels. Set
# `top_axis = TRUE` to leave room above the panels for a second x axis.
# Settings named in `highlight` get their label in the highlight colour, to
# explain what is marked in that colour in the panels.
draw_figure <- function(panels, draw_panel, main, items, footer, key = NULL,
                        shared_x = TRUE, top_axis = FALSE, mar_left = 4.5,
                        highlight = NULL) {
  k <- length(panels)
  n_col <- if (k <= 3) 1 else 2
  n_row <- ceiling(k / n_col)
  cells <- matrix(seq_len(n_row * n_col), n_row, n_col, byrow = TRUE)
  cells[cells > k] <- 0
  cells <- rbind(0, cells, 0)
  widths <- rep(4 / n_col, n_col)
  if (!is.null(items)) {
    cells <- cbind(cells, k + 1)
    widths <- c(widths, 1.3)
  }

  # Strip heights in inches, from the height of a line of text at the scale
  # scale_to_window() will set
  graphics::par(oma = c(1.3, 0, 0, 0))
  line_in <- window_scale() * graphics::par("cin")[2]
  strip <- c(if (top_axis) 6 else 3, if (shared_x) 3.6 else 0.6) * line_in
  inner <- graphics::par("din")[2] - 1.3 * line_in
  # Panels keep at least a third of the height in very small windows
  panel_h <- max(inner - sum(strip), inner / 3) / n_row
  regions <- figure_regions(cells, widths,
                            c(strip[1], rep(panel_h, n_row), strip[2]))
  scale_to_window()
  graphics::par(mar = c(if (shared_x) 0.5 else 3.6, mar_left, 0.5, 1))

  for (i in seq_len(k)) {
    pos <- which(cells == i, arr.ind = TRUE)
    below <- cells[pos[1] + 1, pos[2]]
    use_region(regions[[i]])
    draw_panel(panels[[i]], bottom = !shared_x || below == 0,
               top = top_axis && pos[1] == 2)
  }
  figure_boxes(main, sum(widths[seq_len(n_col)]) / sum(widths))
  plot_footer(footer)
  if (!is.null(items)) {
    use_region(regions[[k + 1]])
    draw_settings_panel(items, highlight = highlight, key = key)
  }
  invisible()
}

# Where each figure goes on the page, like graphics::layout(), as
# c(x0, x1, y0, y1) for par(fig). `cells` numbers the figures (0 is empty),
# and `widths` and `heights` size its columns and rows. The figures are
# placed with par(fig) instead of layout(), because layout() starts a new
# page, and a new page started inside recordGraphics() does not clear the
# previous plot from R's record. Plot viewers then draw every earlier plot
# underneath when they redraw at a new size.
figure_regions <- function(cells, widths, heights) {
  x <- c(0, cumsum(widths)) / sum(widths)
  y <- 1 - c(0, cumsum(heights)) / sum(heights)
  lapply(seq_len(max(cells)), function(f) {
    pos <- which(cells == f, arr.ind = TRUE)
    c(x[min(pos[, 2])], x[max(pos[, 2]) + 1],
      y[max(pos[, 1]) + 1], y[min(pos[, 1])])
  })
}

# Draws the next plot in `region` of the current page, without a new page
use_region <- function(region) {
  graphics::par(fig = region, new = TRUE)
}

# Draws the title in a rounded box at the top, and a rounded box around the
# panels and their axes. Positions are worked out in device units (ndc), then
# drawn from the current panel with xpd = NA. `right` is where the panel
# columns end, as a share of the width.
figure_boxes <- function(main, right) {
  line <- graphics::par("csi") / graphics::par("din")[2] # in ndc
  left <- 0.006
  right <- right - 0.006
  title_top <- 1 - 0.1 * line
  title_bottom <- 1 - 2.5 * line
  panels_top <- title_bottom - 0.4 * line
  panels_bottom <- 1.4 * line

  old <- graphics::par(xpd = NA)
  on.exit(graphics::par(old))
  ux <- function(x) graphics::grconvertX(x, "ndc", "user")
  uy <- function(y) graphics::grconvertY(y, "ndc", "user")
  r <- 0.6 * line * graphics::par("din")[2] / graphics::par("din")[1] *
    diff(ux(c(0, 1)))
  rounded_rect(ux(left), uy(panels_bottom), ux(right), uy(panels_top),
               r = r, col = NA, border = "grey70")
  rounded_rect(ux(left), uy(title_bottom), ux(right), uy(title_top),
               r = r, col = "grey97", border = "grey70")
  graphics::text(ux((left + right) / 2), uy((title_bottom + title_top) / 2),
                 main, font = 2, cex = 1.2)
}

# Panel background: white plot region with faint grid lines at `h` and/or `v`
panel_background <- function(h = NULL, v = NULL) {
  usr <- graphics::par("usr")
  graphics::rect(usr[1], usr[3], usr[2], usr[4], col = "white", border = NA)
  if (!is.null(h)) graphics::abline(h = h, col = "grey92")
  if (!is.null(v)) graphics::abline(v = v, col = "grey92")
}

# Key of lines or filled boxes for the settings panel, one entry per line of
# text, filled down the first column then the second. `key` is a list with
# `labels`, `colours`, `type` ("line" or "box") and optionally a `title`
# drawn above the entries. More than 4 entries go in two columns.
draw_entry_key <- function(key, text_line) {
  n <- length(key$labels)
  rows <- key_rows(key)
  for (i in seq_len(n)) {
    x0 <- if (i > rows) 0.5 else 0.06
    y <- (1.3 + 1.3 * (rows - 1 - (i - 1) %% rows)) * text_line
    if (identical(key$type, "line")) {
      graphics::segments(x0, y, x0 + 0.12, y, col = key$colours[i], lwd = 3)
    } else {
      graphics::rect(x0, y - 0.35 * text_line, x0 + 0.12,
                     y + 0.35 * text_line, col = key$colours[i],
                     border = "grey60")
    }
    graphics::text(x0 + 0.16, y, key$labels[i], adj = c(0, 0.5), cex = 0.7)
  }
  if (!is.null(key$title)) {
    graphics::text(0.06, (1.3 + 1.3 * rows) * text_line, key$title,
                   adj = c(0, 0.5), cex = 0.7, col = "grey35")
  }
}

# Height of a key in lines of text, so the settings panel leaves room for it
key_lines <- function(key) {
  if (is.null(key)) return(0)
  1.2 + 1.3 * key_rows(key) + if (is.null(key$title)) 0 else 1.3
}

# Rows the key takes: one column up to 4 entries, two columns above that
key_rows <- function(key) {
  n <- length(key$labels)
  if (n > 4) ceiling(n / 2) else n
}

# Draws a figure with draw_figure(), keeping the user's graphics settings.
# The page is started first, outside recordGraphics(), so R clears the
# previous plot from its record. The drawing is then wrapped in
# recordGraphics() so plot viewers work it out again at a new size, as the
# boxes, text size and spacing depend on the window size.
plot_figure <- function(panels, draw_panel, main, items, footer, key = NULL,
                        shared_x = TRUE, top_axis = FALSE, mar_left = 4.5,
                        highlight = NULL) {
  old <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old))
  graphics::plot.new()
  grDevices::recordGraphics(
    draw_figure(panels, draw_panel, main = main, items = items,
                footer = footer, key = key, shared_x = shared_x,
                top_axis = top_axis, mar_left = mar_left,
                highlight = highlight),
    list(panels = panels, draw_panel = draw_panel, main = main,
         items = items, footer = footer, key = key, shared_x = shared_x,
         top_axis = top_axis, mar_left = mar_left, highlight = highlight),
    getNamespace("ST4")
  )
}
