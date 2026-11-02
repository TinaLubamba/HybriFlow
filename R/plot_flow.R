# ==============================================================================
# PARTICIPANT FLOW DIAGRAM (internal helper, not exported)
# ==============================================================================

# Points of a rectangle with rounded corners (so the boxes look like a flow chart, no extra packages)
rounded_rect <- function(id, xmin, xmax, ymin, ymax, r = 0.12, n = 8) {
  corner <- function(cx, cy, a0) {
    a <- seq(a0, a0 + pi / 2, length.out = n)
    data.frame(x = cx + r * cos(a), y = cy + r * sin(a))
  }
  pts <- rbind(corner(xmax - r, ymax - r, 0),          corner(xmin + r, ymax - r, pi / 2),
               corner(xmin + r, ymin + r, pi),         corner(xmax - r, ymin + r, 3 * pi / 2))
  pts$id <- id
  pts
}

#' Participant flow diagram
#'
#' Box-and-arrow diagram of how women move through the follow-up analysis: women
#' set aside because their visit dates cannot be read, women with baseline only,
#' women by total number of visits, the group analysed (the women with the full
#' number of visits), and the women of that group who are set aside before the
#' outcome categories (with the reasons).
#'
#' @param n_total Number of women (QC-passed) before any date filtering.
#' @param n_unreadable Number of women whose visit dates cannot be read.
#' @param visits_per_woman Integer vector, one entry per woman with a visit order:
#'   the total number of visits she contributed.
#' @param n_full Number of visits a woman needs to be included (`visits_required`).
#' @param n_outcome Number of women placed in the outcome categories.
#' @param set_aside Named integer vector: reason -> number of women set aside
#'   among the women with `n_full` visits (may be empty).
#' @return A ggplot object.
#' @keywords internal
#' @noRd
plot_participant_flow <- function(n_total, n_unreadable, visits_per_woman, n_full, n_outcome,
                                  set_aside = integer(0)) {
  n_order    <- length(visits_per_woman)
  n_baseline <- sum(visits_per_woman == 1)
  n_fu       <- sum(visits_per_woman >= 2)
  counts     <- table(visits_per_woman[visits_per_woman >= 2])
  ks         <- sort(unique(c(as.integer(names(counts)), n_full)))     # always show the included group
  pct        <- function(n, of = n_order) sprintf("%d (%.1f%%)", n, 100 * n / of)
  set_aside  <- set_aside[set_aside > 0]
  n_aside    <- sum(set_aside)
  
  blue  <- list(fill = "#DCEAF7", line = "#3B7DBF")
  grey  <- list(fill = "#EEEEEE", line = "#7F7F7F")
  white <- list(fill = "#FFFFFF", line = "#7F7F7F")
  green <- list(fill = "#D5E8D4", line = "#2E7D32")
  
  bw <- 2.6; bh <- 1.05
  y  <- c(top = 7.2, order = 5.6, split = 4.0, exact = 2.4, out = 0.8)
  
  boxes <- list()
  add <- function(id, x, y, title, sub = character(0), col, w = bw)
    boxes[[length(boxes) + 1]] <<- list(id = id, x = x, y = y, title = title, sub = sub, col = col, w = w)
  
  add("total", 0, y[["top"]], paste0(n_total, " women"), "(QC-passed samples)", blue)
  add("order", 0, y[["order"]], paste0(n_order, " women"), "with visit order determined", blue)
  has_unread <- n_unreadable > 0
  if (has_unread)
    add("unread", 4.2, (y[["top"]] + y[["order"]]) / 2,
        sprintf("%d women (%.1f%%) excluded", n_unreadable, 100 * n_unreadable / n_total),
        c("unreadable visit dates", "(no visit order)"), grey, w = 3.0)
  xb <- -2.4; xf <- 1.8
  add("base", xb, y[["split"]], pct(n_baseline), "baseline only (excluded)", grey)
  add("fu",   xf, y[["split"]], pct(n_fu), "at least 1 follow-up", white)
  
  k <- length(ks); gap <- bw + 0.35
  xs <- xf + (seq_len(k) - (k + 1) / 2) * gap
  for (i in seq_len(k)) {
    nk <- if (as.character(ks[i]) %in% names(counts)) as.integer(counts[[as.character(ks[i])]]) else 0L
    inc <- ks[i] == n_full
    add(paste0("v", ks[i]), xs[i], y[["exact"]], pct(nk),
        c(paste0(ks[i], " visits in total"), if (inc) "(included)" else "(excluded)"),
        if (inc) blue else grey)
  }
  xi <- xs[which(ks == n_full)]
  has_aside <- n_aside > 0
  x_out <- if (has_aside) xi - 1.7 else xi
  add("out", x_out, y[["out"]], paste0(n_outcome, " women"), "in the outcome categories", green)
  if (has_aside)
    add("aside", xi + 1.9, y[["out"]], paste0(n_aside, " women set aside"),
        paste(set_aside, names(set_aside)), grey, w = 3.1)
  
  # ---- geometry ---------------------------------------------------------------
  B <- do.call(rbind, lapply(boxes, function(b)
    data.frame(id = b$id, x = b$x, y = b$y, w = b$w, fill = b$col$fill, line = b$col$line,
               stringsAsFactors = FALSE)))
  polys <- do.call(rbind, lapply(seq_len(nrow(B)), function(i)
    cbind(rounded_rect(B$id[i], B$x[i] - B$w[i] / 2, B$x[i] + B$w[i] / 2, B$y[i] - bh / 2, B$y[i] + bh / 2),
          fill = B$fill[i], line = B$line[i])))
  
  ls <- 0.23   # line spacing
  txt <- do.call(rbind, lapply(boxes, function(b) {
    lines <- c(b$title, b$sub); n <- length(lines)
    data.frame(x = b$x, y = b$y + ((n - 1) / 2 - (seq_len(n) - 1)) * ls,
               label = lines, face = c("bold", rep("plain", n - 1)), stringsAsFactors = FALSE)
  }))
  
  # ---- connectors ---------------------------------------------------------------
  seg <- function(x, xend, y, yend, arrow = FALSE) data.frame(x = x, xend = xend, y = y, yend = yend, arrow = arrow)
  bx  <- function(id) B[B$id == id, ]
  down <- function(from, to) {          # elbow from the bottom of one box to the top of another
    f <- bx(from); t <- bx(to); mid <- (f$y - bh / 2 + t$y + bh / 2) / 2
    rbind(seg(f$x, f$x, f$y - bh / 2, mid), seg(f$x, t$x, mid, mid),
          seg(t$x, t$x, mid, t$y + bh / 2, arrow = TRUE))
  }
  segs <- rbind(seg(0, 0, y[["top"]] - bh / 2, y[["order"]] + bh / 2, arrow = TRUE),
                down("order", "base"), down("order", "fu"))
  if (has_unread) {
    u <- bx("unread")
    segs <- rbind(segs, seg(0, u$x - u$w / 2, u$y, u$y, arrow = TRUE))
  }
  for (i in seq_len(k)) segs <- rbind(segs, down("fu", paste0("v", ks[i])))
  segs <- rbind(segs, down(paste0("v", n_full), "out"))
  if (has_aside) segs <- rbind(segs, down(paste0("v", n_full), "aside"))
  
  arr <- ggplot2::arrow(length = ggplot2::unit(0.16, "cm"), type = "closed")
  ggplot2::ggplot() +
    ggplot2::geom_segment(data = segs[!segs$arrow, ], ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
                          colour = "grey30", linewidth = 0.45) +
    ggplot2::geom_segment(data = segs[segs$arrow, ], ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
                          colour = "grey30", linewidth = 0.45, arrow = arr) +
    ggplot2::geom_polygon(data = polys, ggplot2::aes(x = x, y = y, group = id, fill = fill, colour = line),
                          linewidth = 0.6) +
    ggplot2::geom_text(data = txt, ggplot2::aes(x = x, y = y, label = label, fontface = face),
                       size = 3.3, colour = "#222222") +
    ggplot2::scale_fill_identity() + ggplot2::scale_colour_identity() +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(add = 0.3)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(add = 0.3)) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(10, 10, 10, 10))
}
