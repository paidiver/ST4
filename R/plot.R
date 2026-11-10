#' Plot a table of sample units required
#'
#' Draws a [sample_size_table()] as a heatmap with CV down the side and delta
#' along the top, laid out like Table 1 of Durden et al. (2016). Each cell
#' shows the number of SUs required per stratum. Darker cells need more SUs.
#'
#' A panel on the right lists the settings the table was made with, so a saved
#' plot explains itself.
#'
#' @param x A `sample_size_table` object from [sample_size_table()].
#' @param max_shown Cells needing more than this are greyed out instead of
#'   coloured. Durden et al. (2016) grey out cells above 10. By default no
#'   cells are greyed out, unless `max_n` is set and cells need more.
#' @param main Plot title.
#' @param strata Optional names of the strata you plan to compare, such as
#'   `c("sand", "hard")`. Only used as a label in the settings panel. There
#'   must be one name per stratum the table was made for (`n_strata` in
#'   [sample_size_control()]).
#' @param highlight Optional CV and delta to mark, such as
#'   `c(cv = 20, delta = 40)`, for example your guessed CV and the difference
#'   you want to detect. The matching cell is outlined, and the panel shows
#'   the sample units needed. Values that are not in the table's grid are
#'   still worked out and shown in the panel, but no cell is outlined.
#' @param settings_panel Show the run settings panel on the right.
#' @param ... Other graphical arguments passed on to [graphics::image()].
#'
#' @return `x`, invisibly.
#'
#' @examples
#' plot(sample_size_table(), max_shown = 10)
#'
#' # Name the strata you plan to compare
#' plot(sample_size_table(), strata = c("sand", "hard"))
#'
#' # Mark your guessed CV and the difference you want to detect
#' plot(sample_size_table(), highlight = c(cv = 20, delta = 40))
#' @export
plot.sample_size_table <- function(x, max_shown = NULL,
                                   main = "Sample units required per stratum",
                                   strata = NULL, highlight = NULL,
                                   settings_panel = TRUE, ...) {
  s <- attr(x, "settings")
  if (!is.null(highlight)) {
    if (!is.numeric(highlight) || length(highlight) != 2 ||
        !setequal(names(highlight), c("cv", "delta"))) {
      stop("`highlight` must be a CV and a delta, for example ",
           "`c(cv = 20, delta = 40)`.", call. = FALSE)
    }
    check_positive(highlight, "highlight")
  }
  if (!is.null(strata)) {
    if (!is.character(strata) || anyNA(strata) ||
        length(strata) != s$n_strata) {
      stop("`strata` must give ", s$n_strata, " names, one per stratum the ",
           "table was made for. To compare ", length(strata), " strata, ",
           "make the table with n_strata = ", length(strata), ".",
           call. = FALSE)
    }
  }
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

  # Where the highlighted CV and delta sit in the grid (NA if not there).
  # Rows are flipped, so the smallest CV is at the top.
  hl_col <- hl_row <- hl_y <- NA
  highlight_text <- NULL
  if (!is.null(highlight)) {
    hl_col <- match(highlight[["delta"]], as.numeric(delta))
    hl_row <- match(highlight[["cv"]], as.numeric(cv))
    hl_y <- length(cv) - hl_row + 1
    on_grid <- !is.na(hl_col) && !is.na(hl_row)
    n_needed <- sample_size(highlight[["cv"]], highlight[["delta"]],
                            control = s)
    highlight_text <- paste0(
      "CV ", highlight[["cv"]], "%, delta ", highlight[["delta"]], "%\n",
      if (is.na(n_needed)) paste0("> ", s$max_n) else n_needed, " SUs",
      if (on_grid) "" else "\n(not on the grid)"
    )
  }

  # Cell labels, with white text on the darker half of the colours
  labels <- t(m[rev(seq_along(cv)), , drop = FALSE])
  labels <- ifelse(is.na(labels), paste0(">", s$max_n), labels)
  level <- (z - lo) / max(1, hi - lo)
  text_col <- ifelse(!is.na(level) & level > 0.5, "white", "black")

  items <- NULL
  key <- NULL
  if (settings_panel) {
    items <- c(
      "Source" = "Eqn 1, set CVs",
      "Survey data" = "not used",
      "Strata compared" = if (is.null(strata)) s$n_strata else
        paste0(s$n_strata, ": ", paste(strata, collapse = ", ")),
      "Alpha" = s$alpha,
      "Power" = s$power,
      "Max n" = if (is.infinite(s$max_n)) "no limit" else s$max_n,
      "Greyed out" = if (is.null(max_shown)) "none" else paste0("> ", max_shown)
    )
    if (!is.null(highlight)) items <- c(items, "Highlighted" = highlight_text)
    if (!all(greyed)) key <- list(colours = colours, low = lo, high = hi)
    if (any(greyed)) {
      key$grey <- if (is.null(max_shown)) paste0("> ", s$max_n) else
        paste0("> ", max_shown)
    }
  }

  dots <- list(...) # extra arguments for image()

  old <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::layout(1)
    graphics::par(old)
  })

  # Draw inside recordGraphics() so that when a plot viewer redraws the plot
  # at a new size (as VS Code and RStudio do), the drawing code runs again.
  # The boxes, text size and panel spacing depend on the window size, so
  # replaying the old drawing commands would put them in the wrong place.
  grDevices::recordGraphics(
    draw_sample_size_plot(
      z = z, cv = cv, delta = delta, greyed = greyed, colours = colours,
      lo = lo, hi = hi, labels = labels, text_col = text_col,
      hl_col = hl_col, hl_row = hl_row, hl_y = hl_y, main = main,
      items = items, key = key,
      highlighted = !is.null(highlight), image_args = dots
    ),
    list(z = z, cv = cv, delta = delta, greyed = greyed, colours = colours,
         lo = lo, hi = hi, labels = labels, text_col = text_col,
         hl_col = hl_col, hl_row = hl_row, hl_y = hl_y, main = main,
         items = items, key = key,
         highlight = highlight, dots = dots),
    getNamespace("ST4")
  )

  invisible(x)
}

# Draws the whole heatmap figure. Everything that depends on the window size
# is worked out here, so it is right every time the plot is redrawn.
draw_sample_size_plot <- function(z, cv, delta, greyed, colours, lo, hi,
                                  labels, text_col, hl_col, hl_row, hl_y,
                                  main, items, key,
                                  highlighted, image_args) {
  # Outer margin at the bottom for the footer
  graphics::par(oma = c(1.3, 0, 0, 0))
  if (!is.null(items)) {
    graphics::layout(matrix(1:2, nrow = 1), widths = c(4, 1.3))
  } else {
    graphics::layout(1)
  }
  scale_to_window()
  graphics::par(mar = c(1.6, 4.5, 6.5, 1))

  graphics::plot.new()
  graphics::plot.window(xlim = c(0.5, length(delta) + 0.5),
                        ylim = c(0.5, length(cv) + 0.5), xaxs = "i", yaxs = "i")

  # Rounded boxes behind the title and behind the heatmap with its axes. Their
  # tops and bottoms line up with the settings panel. White behind the
  # heatmap so its palest cells still stand out.
  margin_box(left = 4.2, right = 0.4, bottom = -0.6, top = 3.9, col = "white")
  margin_box(left = 4.2, right = 0.4, bottom = 4.3, top = 5.9)

  # Grey background first, so greyed (NA) cells show through the heatmap
  graphics::rect(0.5, 0.5, length(delta) + 0.5, length(cv) + 0.5,
                 col = "grey85", border = NA)
  if (!all(greyed)) {
    do.call(graphics::image,
            c(list(seq_along(delta), seq_along(cv), z, col = colours,
                   zlim = c(lo, hi), add = TRUE), image_args))
  }
  graphics::title(ylab = "CV (%)")

  # Crosshair: faint band along the highlighted row and column
  band <- grDevices::adjustcolor(highlight_colour, alpha.f = 0.1)
  if (!is.na(hl_col)) {
    graphics::rect(hl_col - 0.5, 0.5, hl_col + 0.5, length(cv) + 0.5,
                   col = band, border = NA)
  }
  if (!is.na(hl_row)) {
    graphics::rect(0.5, hl_y - 0.5, length(delta) + 0.5, hl_y + 0.5,
                   col = band, border = NA)
  }

  # Axis labels, with the highlighted CV and delta in bold orange
  draw_axis_labels(3, seq_along(delta), delta, hl_col)
  draw_axis_labels(2, seq_along(cv), rev(cv),
                   if (is.na(hl_row)) NA else hl_y, las = 1)
  # mtext() sizes are absolute, so scale them with the window like other text
  graphics::mtext("Delta (%)", side = 3, line = 2.2,
                  cex = graphics::par("cex"))
  graphics::title(main, line = 4.75)

  graphics::text(rep(seq_along(delta), length(cv)),
                 rep(seq_along(cv), each = length(delta)),
                 labels, col = text_col, cex = 0.8)

  # Outline the highlighted cell, if its CV and delta are in the grid
  if (!is.na(hl_col) && !is.na(hl_row)) {
    graphics::rect(hl_col - 0.5, hl_y - 0.5, hl_col + 0.5, hl_y + 0.5,
                   border = highlight_colour, lwd = 4)
  }

  # No survey data goes into this table, so the footer gives the equation
  # that made it instead of a data citation
  plot_footer(paste("Made with Durden et al. (2016) Eqn 1, from Sokal &",
                    "Rohlf (1995), for assumed CVs. No survey data used."))

  if (!is.null(items)) {
    draw_settings_panel(items,
                        highlight = if (highlighted) "Highlighted",
                        key = key)
  }
  invisible()
}

# Colour for highlighted cells and their entry in the settings panel
highlight_colour <- "#E8590C"

# Draws a boxed list of run settings filling the current plot region, like a
# legend. `items` is a named vector: names are labels, values are settings.
# Items named in `highlight` get their label in the highlight colour.
# `key`, if given, is a list with `colours`, `low` and `high` (and optionally
# `grey`, the label for greyed-out cells), drawn as a colour bar at the bottom.
draw_settings_panel <- function(items, title = "Run settings",
                                highlight = NULL, key = NULL) {
  # Small top margin so the panel runs from the top of the title down to the
  # bottom of the plot
  graphics::par(mar = c(1, 0.5, 0.6, 1))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1), xaxs = "i",
                        yaxs = "i")

  # Long values wrap onto extra lines so they stay inside the box. A "\n" in
  # a value starts a new line.
  values <- lapply(items, function(v) {
    parts <- strsplit(paste(format(v), collapse = ", "), "\n", fixed = TRUE)
    unlist(lapply(parts[[1]], strwrap, width = 18))
  })
  # Same spacing as the loop below: title, then per setting a label and its
  # value lines, with a gap between settings and a margin under the last one.
  # Lines get closer together if needed so the box fits the plot height.
  height_in_lines <- 2 + sum(0.85 * lengths(values)) +
    1.15 * (length(items) - 1) + 0.7
  # Spacing is set in lines of text, not as a share of the panel height, so
  # it stays the same however big the window is. `text_line` is the height of
  # one line of text in panel units (the panel runs from 0 to 1).
  text_line <- graphics::par("csi") / graphics::par("pin")[2]
  # Room at the bottom for the colour key
  key_h <- text_line *
    (if (is.null(key)) 0 else if (is.null(key$grey)) 3.9 else 5.4)
  line_h <- min(1.3 * text_line, (0.99 - key_h) / height_in_lines)
  top <- 1
  rounded_rect(0.01, 0.002, 0.99, top - 0.002, col = "grey97",
               border = "grey70")
  graphics::text(0.06, top - line_h * 0.8, title, adj = c(0, 0.5), font = 2,
                 cex = 0.85)

  # Each setting: label in grey, value below in black
  y <- top - line_h * 2
  for (i in seq_along(items)) {
    label_col <- if (names(items)[i] %in% highlight) highlight_colour else
      "grey35"
    graphics::text(0.06, y, names(items)[i], adj = c(0, 0.5), cex = 0.7,
                   col = label_col, font = if (label_col == "grey35") 1 else 2)
    for (line in values[[i]]) {
      y <- y - line_h * 0.85
      graphics::text(0.06, y, line, adj = c(0, 0.5), cex = 0.8)
    }
    y <- y - line_h * 1.15
  }

  if (!is.null(key)) draw_colour_key(key, text_line)
  invisible()
}

# Colour bar for the settings panel: fewest SUs on the left, most on the
# right, with a grey swatch for greyed-out cells if there are any
# Positions are in lines of text (`text_line`, in panel units), so the key
# keeps its shape in any window size
draw_colour_key <- function(key, text_line) {
  x0 <- 0.06
  x1 <- 0.94
  bar_y <- 1.7 * text_line
  bar_h <- 0.78 * text_line
  # No colour bar if every cell is greyed out, only the grey swatch
  if (!is.null(key$colours)) {
    draw_colour_bar(key, x0, x1, bar_y, bar_h, text_line)
  }

  if (!is.null(key$grey)) {
    sw_y <- bar_y + bar_h + 1.55 * text_line
    graphics::rect(x0, sw_y, x0 + 0.08, sw_y + bar_h, col = "grey85",
                   border = "grey60")
    graphics::text(x0 + 0.11, sw_y + bar_h / 2,
                   paste0("Greyed out (", key$grey, ")"),
                   adj = c(0, 0.5), cex = 0.7)
  }
}

draw_colour_bar <- function(key, x0, x1, bar_y, bar_h, text_line) {
  graphics::text(x0, bar_y + bar_h + 0.65 * text_line, "SUs needed",
                 adj = c(0, 0.5),
                 cex = 0.7, col = "grey35")
  n <- length(key$colours)
  edges <- seq(x0, x1, length.out = n + 1)
  # Segment borders in their own colour, so no gaps show between them
  graphics::rect(edges[-(n + 1)], bar_y, edges[-1], bar_y + bar_h,
                 col = key$colours, border = key$colours, lwd = 0.5)
  graphics::rect(x0, bar_y, x1, bar_y + bar_h, border = "grey60")
  graphics::text(x1, bar_y - 0.57 * text_line, key$high, adj = c(1, 0.5),
                 cex = 0.7)
  graphics::text(x0, bar_y - 0.57 * text_line, key$low, adj = c(0, 0.5),
                 cex = 0.7)
}

# Axis labels with one optional label (at position `hl`) in bold orange
draw_axis_labels <- function(side, at, labels, hl, ...) {
  plain <- labels
  if (!is.na(hl)) plain[at == hl] <- ""
  graphics::axis(side, at = at, labels = plain, tick = FALSE, ...)
  if (!is.na(hl)) {
    graphics::axis(side, at = hl, labels = labels[at == hl], tick = FALSE,
                   col.axis = highlight_colour, font = 2, ...)
  }
}

# A rectangle with rounded corners, drawn as a polygon. The corner radius is
# a share of the plot width and is scaled to look round in the plot's aspect
# ratio.
rounded_rect <- function(x0, y0, x1, y1, r = 0.06, col = NA, border = "black",
                         n = 10) {
  usr <- graphics::par("usr")
  pin <- graphics::par("pin")
  # Data units per inch on each axis, so the corners are circles, not ovals
  ry <- r * (diff(usr[3:4]) / pin[2]) / (diff(usr[1:2]) / pin[1])
  r <- min(r, (x1 - x0) / 2)
  ry <- min(ry, (y1 - y0) / 2)

  corner <- function(cx, cy, from) {
    a <- seq(from, from + pi / 2, length.out = n)
    list(x = cx + r * cos(a), y = cy + ry * sin(a))
  }
  parts <- list(
    corner(x1 - r, y1 - ry, 0),          # top right
    corner(x0 + r, y1 - ry, pi / 2),     # top left
    corner(x0 + r, y0 + ry, pi),         # bottom left
    corner(x1 - r, y0 + ry, 3 * pi / 2)  # bottom right
  )
  graphics::polygon(unlist(lapply(parts, `[[`, "x")),
                    unlist(lapply(parts, `[[`, "y")),
                    col = col, border = border)
}

# A rounded box partly in the plot margins, placed in lines of text from the
# edges of the plot region: `left` and `right` lines out from the sides,
# `bottom` and `top` lines above the top edge (negative is below the bottom
# edge for `bottom`).
margin_box <- function(left, right, bottom, top, col = "grey97",
                       border = "grey70") {
  usr <- graphics::par("usr")
  pin <- graphics::par("pin")
  line_in <- graphics::par("csi") # height of a line of text, in inches
  per_line_x <- line_in * diff(usr[1:2]) / pin[1]
  per_line_y <- line_in * diff(usr[3:4]) / pin[2]

  y0 <- if (bottom < 0) usr[3] + bottom * per_line_y else
    usr[4] + bottom * per_line_y
  old <- graphics::par(xpd = NA)
  on.exit(graphics::par(old))
  rounded_rect(usr[1] - left * per_line_x, y0,
               usr[2] + right * per_line_x, usr[4] + top * per_line_y,
               r = 0.6 * per_line_x, col = col, border = border)
}

# Small grey line along the bottom of the whole figure: `text` on the left
# (the data citation for plots of data, or the method for the heatmap) and the
# package version on the right. Needs an outer margin at the bottom
# (par(oma)).
plot_footer <- function(text) {
  pkg <- utils::packageName()
  size <- 0.65 * graphics::par("cex") # mtext() sizes are absolute
  graphics::mtext(text, side = 1, line = 0.2, outer = TRUE, adj = 0.005,
                  cex = size, col = "grey45")
  graphics::mtext(paste(pkg, utils::packageVersion(pkg)), side = 1,
                  line = 0.2, outer = TRUE, adj = 0.995, cex = size,
                  col = "grey45")
}


# Scales all text and margins with the size of the plot window, so a big
# window looks like a larger copy of the plot and a small one like a smaller
# copy. The plot is laid out for 9 by 5.75 inches. The scale is kept between
# 0.75 and 1.6 so text stays readable.
scale_to_window <- function() {
  din <- graphics::par("din")
  scale <- min(din[1] / 9, din[2] / 5.75)
  scale <- min(max(scale, 0.75), 1.6)
  # Margin lines are measured in lines of text, so changing cex scales the
  # margins too. Setting mex as well would scale them twice.
  graphics::par(cex = scale)
}
