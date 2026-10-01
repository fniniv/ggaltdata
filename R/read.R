# Reading a ggplot into a tidy description of what is drawn -----------------------------------
#
# Everything downstream works on the object returned by gad_read(): one standard table per data
# layer (category, group, value, interval, series position, panel, area) plus the labels of the plot.
# Values come from ggplot_build(), i.e. from what is actually drawn; positions on discrete axes are
# mapped back to their category labels and fill/colour back to their group labels.

GEOM_KIND <- c(
  # computed shapes that inherit from supported geoms but are not values to read
  GeomDensity = "unsupported", GeomSmooth = "unsupported", GeomViolin = "unsupported",
  GeomCol = "bar", GeomBar = "bar",
  GeomPoint = "point",
  GeomLine = "line", GeomPath = "line", GeomArea = "line", GeomStep = "line",
  GeomPointrange = "interval", GeomErrorbar = "interval", GeomLinerange = "interval",
  GeomCrossbar = "interval", GeomErrorbarh = "interval",
  GeomHline = "ref", GeomVline = "ref",
  GeomSegment = "segment",
  GeomSf = "sf",
  GeomText = "annotation", GeomLabel = "annotation", GeomBlank = "annotation",
  # a confidence band drawn around a line: the line carries the findings
  GeomRibbon = "annotation"
)

geom_kind <- function(layer) {
  cl <- class(layer$geom)
  hit <- cl[cl %in% names(GEOM_KIND)]
  if (length(hit)) GEOM_KIND[[hit[1]]] else "unsupported"
}

is_discrete_scale <- function(s) !is.null(s) && inherits(s, "ScaleDiscrete")
time_kind <- function(s) {
  if (is.null(s)) "none" else if (inherits(s, "ScaleContinuousDatetime")) "datetime"
  else if (inherits(s, "ScaleContinuousDate")) "date" else "none"
}

# the coarsest unit that names every x of a series without losing information
time_unit <- function(x, time, tz) {
  if (time == "date") {
    if (all(format(as.Date(x, origin = "1970-01-01"), "%d") == "01")) "month" else "day"
  } else if (time == "datetime") {
    t <- as.POSIXct(x, origin = "1970-01-01", tz = tz)
    if (all(format(t, "%H:%M:%S") == "00:00:00")) "day" else "minute"
  } else "none"
}

as_time <- function(x, time, tz) {
  if (time == "datetime") as.POSIXct(x, origin = "1970-01-01", tz = tz) else as.Date(x, origin = "1970-01-01")
}

# map the colours of a discrete fill/colour scale back to their labels
colour_to_label <- function(plot_scales, aes, values) {
  s <- plot_scales$get_scales(aes)
  if (is.null(s) || !inherits(s, "ScaleDiscrete")) return(rep(NA_character_, length(values)))
  lim <- s$get_limits()
  pal <- s$map(lim)
  as.character(lim[match(values, pal)])
}

panel_labels <- function(layout) {
  lay <- layout$layout
  vars <- setdiff(names(lay), c("PANEL", "ROW", "COL", "SCALE_X", "SCALE_Y", "COORD"))
  if (!length(vars)) return(stats::setNames(rep(NA_character_, nrow(lay)), lay$PANEL))
  lab <- apply(lay[vars], 1, function(r) paste(trimws(as.character(r)), collapse = ", "))
  stats::setNames(lab, lay$PANEL)
}

# axis that carries categories ("x", "y") or NA when both axes are continuous
category_axis <- function(layout) {
  sx <- layout$panel_scales_x[[1]]
  sy <- layout$panel_scales_y[[1]]
  if (is_discrete_scale(sx)) return("x")
  if (is_discrete_scale(sy)) return("y")
  NA_character_
}

pos_to_category <- function(pos, scale) {
  lim <- scale$get_limits()
  i <- round(pos)
  out <- rep(NA_character_, length(pos))
  ok <- !is.na(i) & i >= 1 & i <= length(lim)
  out[ok] <- as.character(lim[i[ok]])
  out
}

# evaluate an aesthetic of a layer on its data (used for geom_sf, where fill colours cannot be inverted)
eval_aes <- function(p, layer, aes) {
  map <- c(as.list(p$mapping), as.list(layer$mapping))
  q <- map[[aes]]
  if (is.null(q)) return(NULL)
  data <- if (is.data.frame(layer$data) && nrow(layer$data)) layer$data else p$data
  rlang::eval_tidy(q, data)
}

gad_read <- function(p, area_var = NULL) {
  if (!inherits(p, "ggplot")) {
    rlang::abort("`p` must be a ggplot object.", class = "ggaltdata_error")
  }
  if (!length(p$layers)) rlang::abort("The plot has no layers.", class = "ggaltdata_error")
  b <- suppressWarnings(ggplot2::ggplot_build(p))
  labs <- ggplot2::get_labs(p)
  lay <- b$layout
  cat_ax <- category_axis(lay)
  val_ax <- if (is.na(cat_ax)) "y" else setdiff(c("x", "y"), cat_ax)
  plab <- panel_labels(lay)
  kinds <- vapply(p$layers, geom_kind, character(1))
  layers <- vector("list", length(p$layers))

  for (i in seq_along(p$layers)) {
    d <- b$data[[i]]
    k <- kinds[i]
    if (!nrow(d) || k %in% c("annotation", "segment", "unsupported")) next
    panel <- as.character(d$PANEL)
    out <- data.frame(panel = panel, panel_label = unname(plab[panel]), stringsAsFactors = FALSE)
    grp <- if (!is.null(d$fill) && any(!is.na(colour_to_label(b$plot$scales, "fill", d$fill)))) {
      colour_to_label(b$plot$scales, "fill", d$fill)
    } else if (!is.null(d$colour)) {
      colour_to_label(b$plot$scales, "colour", d$colour)
    } else NA_character_
    out$group <- grp

    if (k == "ref") {
      out$value <- if (!is.null(d$yintercept)) d$yintercept else d$xintercept
      out$ref_axis <- if (!is.null(d$yintercept)) "y" else "x"
    } else if (k == "sf") {
      vals <- eval_aes(p, p$layers[[i]], "fill")
      data <- if (is.data.frame(p$layers[[i]]$data) && nrow(p$layers[[i]]$data)) p$layers[[i]]$data else p$data
      nm <- if (!is.null(area_var) && area_var %in% names(data)) data[[area_var]] else {
        cc <- names(data)[vapply(data, function(z) is.character(z) || is.factor(z), logical(1))]
        if (length(cc)) data[[cc[1]]] else paste("area", seq_len(nrow(data)))
      }
      out <- data.frame(panel = "1", panel_label = NA_character_, group = NA_character_,
                        area = as.character(nm), value = if (is.null(vals)) NA_real_ else as.numeric(vals),
                        stringsAsFactors = FALSE)
    } else if (!is.na(cat_ax) && k %in% c("bar", "point", "interval")) {
      # each panel can have its own scale (facet_wrap(scales = "free_y")): read the panel's scale
      idx <- lay$layout[[if (cat_ax == "x") "SCALE_X" else "SCALE_Y"]][match(d$PANEL, lay$layout$PANEL)]
      scales <- if (cat_ax == "x") lay$panel_scales_x else lay$panel_scales_y
      out$category <- vapply(seq_len(nrow(d)), function(j) pos_to_category(d[[cat_ax]][j], scales[[idx[j]]]), "")
      lo <- paste0(val_ax, "min")
      hi <- paste0(val_ax, "max")
      if (k == "bar") {
        # a bar below zero (ymax <= 0) keeps its sign: its height alone would turn -0.46 into 0.46
        h <- d[[hi]] - d[[lo]]
        out$value <- ifelse(d[[hi]] <= 0 & d[[lo]] < 0, -h, h)
        out$stack_base <- d[[lo]]
        out$stack_top <- d[[hi]]
      } else {
        out$value <- d[[val_ax]]
        if (k == "interval") {
          out$lower <- d[[lo]]
          out$upper <- d[[hi]]
          if (all(is.na(out$value))) out$value <- (out$lower + out$upper) / 2   # errorbar alone
        }
      }
    } else if (is.na(cat_ax) && k %in% c("line", "point", "interval")) {
      if (k == "point" && !any(kinds == "line")) {
        k <- "scatter"
      }
      sx <- lay$panel_scales_x[[1]]
      out$x <- d$x
      # dates are stored as days and date-times as seconds since 1970: keep which one, and the time zone
      out$x_time <- time_kind(sx)
      out$x_tz <- if (out$x_time[1] == "datetime") (sx$timezone %||% "UTC") else NA_character_
      out$value <- d$y
      if (k == "interval") {
        out$lower <- d$ymin
        out$upper <- d$ymax
      }
      if (all(is.na(out$group))) out$group <- if (length(unique(d$group)) > 1) paste("series", d$group) else NA_character_
    } else {
      k <- "unsupported"
    }
    out$kind <- k
    kinds[i] <- k
    layers[[i]] <- out
  }

  pct <- FALSE
  pp <- lay$panel_params[[1]]
  if (!is.null(pp[[val_ax]]) && is.function(pp[[val_ax]]$get_labels)) {
    labs_v <- tryCatch(pp[[val_ax]]$get_labels(), error = function(e) character())
    pct <- any(grepl("%", labs_v, fixed = TRUE))
  }
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  list(plot = p, build = b, layers = layers, kinds = kinds, geoms = geoms, labels = labs,
       cat_axis = cat_ax, value_axis = val_ax, percent = pct,
       cat_label = if (!is.na(cat_ax)) labs[[cat_ax]] else NULL,
       value_label = labs[[val_ax]] %||% labs$fill,
       group_label = labs$fill %||% labs$colour,
       x_label = labs$x, n_panels = nrow(lay$layout))
}

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && (is.na(a) || identical(a, "")))) b else a
