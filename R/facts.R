# Facts computed from the read plot -----------------------------------------------------------
#
# A fact is a list with a `type`, the `panel` it belongs to, a `priority` (1 = goes first in the
# short text) and the values the templates need. No fact contains a number that is not in the data.

primary_layer <- function(r) {
  order <- c("interval", "bar", "point", "line", "sf")
  for (k in order) {
    i <- which(r$kinds == k)
    if (length(i)) return(r$layers[[i[1]]])
  }
  NULL
}

ref_values <- function(r) {
  v <- numeric()
  for (i in which(r$kinds == "ref")) {
    l <- r$layers[[i]]
    if (!is.null(l$ref_axis)) v <- c(v, l$value[l$ref_axis == r$value_axis])
  }
  unique(v)
}

ranked <- function(d) {
  d <- d[!is.na(d$value), , drop = FALSE]
  d[order(-d$value), , drop = FALSE]
}

facts_categories <- function(d, panel, n_extremes, diverging = FALSE) {
  out <- list()
  groups <- unique(stats::na.omit(d$group))
  stacked <- !is.null(d$stack_base) && any(d$stack_base > 0, na.rm = TRUE) && length(groups) > 1
  if (stacked) {
    tot <- stats::aggregate(value ~ category, data = d, FUN = sum)
    tot <- tot[order(-tot$value), ]
    out[[length(out) + 1]] <- list(type = "extremes", panel = panel, priority = 1, what = "total",
      hi = tot$category[1], hi_v = tot$value[1], lo = tot$category[nrow(tot)], lo_v = tot$value[nrow(tot)],
      n = nrow(tot), ranking = tot)
    share <- stats::aggregate(value ~ group, data = d, FUN = sum)
    share$share <- share$value / sum(share$value)
    share <- share[order(-share$share), ]
    out[[length(out) + 1]] <- list(type = "largest_part", panel = panel, priority = 2,
      group = share$group[1], share = share$share[1])
    return(out)
  }
  if (length(groups) == 2) {
    w <- stats::reshape(d[!is.na(d$group), c("category", "group", "value")], idvar = "category",
                        timevar = "group", direction = "wide")
    names(w) <- sub("^value\\.", "", names(w))
    w <- w[stats::complete.cases(w), , drop = FALSE]
    if (nrow(w)) {
      m <- colMeans(w[groups])
      hi_g <- names(m)[which.max(m)]
      lo_g <- setdiff(groups, hi_g)
      gap <- w[[hi_g]] - w[[lo_g]]
      j <- which.max(abs(gap))
      out[[length(out) + 1]] <- list(type = "group_gap", panel = panel, priority = 1,
        hi_g = hi_g, lo_g = lo_g, k = sum(gap > 0), n = length(gap),
        cat_max = w$category[j], gap_max = gap[j], all_same = all(gap > 0))
    }
  }
  if (length(groups) > 2) {
    m <- tapply(d$value, d$group, mean, na.rm = TRUE)
    m <- sort(m, decreasing = TRUE)
    out[[length(out) + 1]] <- list(type = "group_means", panel = panel, priority = 1,
      hi_g = names(m)[1], hi_v = m[[1]], lo_g = names(m)[length(m)], lo_v = m[[length(m)]], n = length(m))
  }
  if (length(groups) <= 1) {
    rk <- ranked(d)
    if (nrow(rk)) {
      # in a chart of differences the direction is the first message, the extremes come second
      sign_pri <- if (diverging) 1 else 2
      neg <- all(rk$value < 0)
      out[[length(out) + 1]] <- list(type = if (neg) "extremes_negative" else "extremes", panel = panel,
        priority = if (diverging) 2 else 1, what = "value",
        hi = rk$category[1], hi_v = rk$value[1], lo = rk$category[nrow(rk)], lo_v = rk$value[nrow(rk)],
        n = nrow(rk), ranking = rk[c("category", "value")],
        top = utils::head(rk$category, n_extremes), bottom = utils::tail(rk$category, n_extremes))
      if (any(rk$value < 0) && any(rk$value > 0)) {
        out[[length(out) + 1]] <- list(type = "sign", panel = panel, priority = sign_pri,
          k = sum(rk$value > 0), n = nrow(rk))
      } else if (neg) {
        out[[length(out) + 1]] <- list(type = "all_negative", panel = panel, priority = sign_pri, n = nrow(rk))
      } else if (diverging) {
        out[[length(out) + 1]] <- list(type = "all_positive", panel = panel, priority = sign_pri, n = nrow(rk))
      }
    }
  }
  out
}

facts_intervals <- function(d, panel, refs) {
  rk <- ranked(d)
  if (!nrow(rk)) return(list())
  hi <- rk[1, ]
  lo <- rk[nrow(rk), ]
  out <- list(list(type = "interval_extremes", panel = panel, priority = 1,
    hi = hi$category, hi_v = hi$value, hi_l = hi$lower, hi_u = hi$upper,
    lo = lo$category, lo_v = lo$value, lo_l = lo$lower, lo_u = lo$upper,
    overlap = !(hi$lower > lo$upper), n = nrow(rk), ranking = rk[c("category", "value", "lower", "upper")]))
  for (r in refs) {
    out[[length(out) + 1]] <- list(type = "interval_ref", panel = panel, priority = 2, ref = r,
      k = sum(rk$lower > r | rk$upper < r), n = nrow(rk),
      above = sum(rk$lower > r), below = sum(rk$upper < r))
  }
  out
}

facts_series <- function(d, panel, tol_share = 0.05) {
  d <- d[!is.na(d$value), , drop = FALSE]
  if (!nrow(d)) return(list())
  span <- diff(range(d$value))
  tol <- tol_share * if (span > 0) span else max(abs(d$value), 1)
  d$series <- ifelse(is.na(d$group), "", d$group)
  out <- list()
  ends <- list()
  for (s in unique(d$series)) {
    z <- d[d$series == s, , drop = FALSE]
    z <- z[order(z$x), , drop = FALSE]
    ch <- z$value[nrow(z)] - z$value[1]
    dir <- if (abs(ch) <= tol) "stable" else if (ch > 0) "up" else "down"
    pk <- which.max(z$value)
    out[[length(out) + 1]] <- list(type = "trend", panel = panel, priority = 1, series = s,
      x0 = z$x[1], x1 = z$x[nrow(z)], v0 = z$value[1], v1 = z$value[nrow(z)], change = ch,
      direction = dir, peak_x = z$x[pk], peak_v = z$value[pk], is_date = isTRUE(z$x_is_date[1]),
      peak_inside = pk > 1 && pk < nrow(z))
    ends[[s]] <- z$value[nrow(z)]
  }
  if (length(ends) > 1) {
    e <- unlist(ends)
    out[[length(out) + 1]] <- list(type = "series_end", panel = panel, priority = 2,
      hi = names(e)[which.max(e)], hi_v = max(e), lo = names(e)[which.min(e)], lo_v = min(e))
  }
  out
}

facts_sf <- function(d, n_extremes) {
  rk <- ranked(d)
  if (!nrow(rk)) return(list())
  list(list(type = "area_extremes", panel = "1", priority = 1,
    hi = rk$area[1], hi_v = rk$value[1], lo = rk$area[nrow(rk)], lo_v = rk$value[nrow(rk)],
    median = stats::median(rk$value), n = nrow(rk), ranking = rk[c("area", "value")],
    top = utils::head(rk$area, n_extremes), bottom = utils::tail(rk$area, n_extremes)))
}

gad_facts <- function(r, n_extremes = 3) {
  d <- primary_layer(r)
  if (is.null(d)) return(list())
  refs <- ref_values(r)
  k <- d$kind[1]
  # a chart of differences: values on both sides of zero, a reference line at zero, or segments
  # drawn from zero (lollipop)
  diverging <- (any(d$value < 0, na.rm = TRUE)) || any(refs == 0) || any(r$kinds == "segment")
  out <- list()
  for (pn in unique(d$panel)) {
    z <- d[d$panel == pn, , drop = FALSE]
    pl <- z$panel_label[1]
    f <- switch(k,
      interval = if (!is.null(z$category)) facts_intervals(z, pl, refs) else facts_series(z, pl),
      bar = , point = facts_categories(z, pl, n_extremes, diverging),
      line = facts_series(z, pl),
      sf = facts_sf(z, n_extremes),
      list())
    out <- c(out, f)
  }
  out
}
