# Text: templates in English and Italian, number formatting, sentences --------------------------
#
# Templates use {placeholders}. They are kept in one list per language so that a new language is a
# new list, not new code.

TEMPLATES <- list(
  en = list(
    extremes = "{hi} has the highest {what} ({hi_v}) and {lo} the lowest ({lo_v}).",
    extremes_total = "{hi} has the highest total ({hi_v}) and {lo} the lowest ({lo_v}).",
    top_bottom = "Highest: {top}. Lowest: {bottom}.",
    largest_part = "{group} is the largest part overall ({share}).",
    group_gap_all = "{hi_g} is higher than {lo_g} on all {n} {cats}; the widest gap is {cat_max} ({gap_max}).",
    group_gap_some = "{hi_g} is higher than {lo_g} on {k} of {n} {cats}; the widest gap is {cat_max} ({gap_max}).",
    group_means = "On average {hi_g} is highest ({hi_v}) and {lo_g} lowest ({lo_v}).",
    sign = "{k} of {n} values are above zero.",
    all_negative = "All {n} values are below zero.",
    all_positive = "All {n} values are above zero.",
    extremes_negative = "{lo} has the largest negative {what} ({lo_v}) and {hi} the smallest ({hi_v}).",
    interval_overlap = "{hi} has the highest estimate ({hi_v}; {hi_l} to {hi_u}) and {lo} the lowest ({lo_v}; {lo_l} to {lo_u}); the two intervals overlap, so the difference is uncertain.",
    interval_apart = "{hi} has the highest estimate ({hi_v}; {hi_l} to {hi_u}) and {lo} the lowest ({lo_v}; {lo_l} to {lo_u}); the two intervals do not overlap.",
    interval_ref = "{k} of {n} intervals exclude the reference value {ref} ({above} above, {below} below).",
    trend_up = "{series}rises from {v0} in {x0} to {v1} in {x1}.",
    trend_down = "{series}falls from {v0} in {x0} to {v1} in {x1}.",
    trend_stable = "{series}stays around {v0} from {x0} to {x1}.",
    peak = "{series}peaks at {peak_v} in {peak_x}.",
    series_end = "At the end, {hi} is highest ({hi_v}) and {lo} lowest ({lo_v}).",
    area_extremes = "{hi} has the highest {what} ({hi_v}) and {lo} the lowest ({lo_v}); the median across {n} areas is {median}.",
    in_panel = "In {panel}: ",
    ranking = "From highest to lowest: {items}.",
    structure = "{type} of {value}{by}{groups}{panels}.",
    by = " by {cat}",
    groups = ", for each {group} ({levels})",
    panels = ", in {n} panels",
    source = "Values are read from the data of the chart.",
    missing = "{n} missing values are not shown.", missing_1 = "1 missing value is not shown.",
    unsupported = "This chart type ({geoms}) is not supported for automatic findings; describe the main pattern by hand.",
    type_bar = "Bar chart", type_point = "Dot chart", type_interval = "Chart of estimates with intervals",
    type_line = "Line chart", type_sf = "Map", type_other = "Chart",
    value = "value", and = "and", series_sep = ": ", categories = "categories"
  ),
  it = list(
    extremes = "{hi} ha il valore pi\u00f9 alto di {what} ({hi_v}), {lo} il pi\u00f9 basso ({lo_v}).",
    extremes_total = "{hi} ha il totale pi\u00f9 alto ({hi_v}), {lo} il pi\u00f9 basso ({lo_v}).",
    top_bottom = "Pi\u00f9 alti: {top}. Pi\u00f9 bassi: {bottom}.",
    largest_part = "{group} \u00e8 la parte pi\u00f9 grande nel complesso ({share}).",
    group_gap_all = "{hi_g} \u00e8 pi\u00f9 alto di {lo_g} in tutte le {n} voci; la distanza maggiore \u00e8 in {cat_max} ({gap_max}).",
    group_gap_some = "{hi_g} \u00e8 pi\u00f9 alto di {lo_g} in {k} voci su {n}; la distanza maggiore \u00e8 in {cat_max} ({gap_max}).",
    group_means = "In media {hi_g} \u00e8 il pi\u00f9 alto ({hi_v}) e {lo_g} il pi\u00f9 basso ({lo_v}).",
    sign = "{k} valori su {n} sono sopra lo zero.",
    all_negative = "Tutti i {n} valori sono sotto lo zero.",
    all_positive = "Tutti i {n} valori sono sopra lo zero.",
    extremes_negative = "{lo} ha il valore negativo pi\u00f9 ampio di {what} ({lo_v}), {hi} il pi\u00f9 piccolo ({hi_v}).",
    interval_overlap = "{hi} ha la stima pi\u00f9 alta ({hi_v}; da {hi_l} a {hi_u}), {lo} la pi\u00f9 bassa ({lo_v}; da {lo_l} a {lo_u}); i due intervalli si sovrappongono, quindi la differenza \u00e8 incerta.",
    interval_apart = "{hi} ha la stima pi\u00f9 alta ({hi_v}; da {hi_l} a {hi_u}), {lo} la pi\u00f9 bassa ({lo_v}; da {lo_l} a {lo_u}); i due intervalli non si sovrappongono.",
    interval_ref = "{k} intervalli su {n} escludono il valore di riferimento {ref} ({above} sopra, {below} sotto).",
    trend_up = "{series}sale da {v0} ({x0}) a {v1} ({x1}).",
    trend_down = "{series}scende da {v0} ({x0}) a {v1} ({x1}).",
    trend_stable = "{series}resta intorno a {v0} da {x0} a {x1}.",
    peak = "{series}tocca il massimo, {peak_v}, in {peak_x}.",
    series_end = "Alla fine {hi} \u00e8 il pi\u00f9 alto ({hi_v}) e {lo} il pi\u00f9 basso ({lo_v}).",
    area_extremes = "{hi} ha il valore pi\u00f9 alto di {what} ({hi_v}), {lo} il pi\u00f9 basso ({lo_v}); la mediana delle {n} aree \u00e8 {median}.",
    in_panel = "In {panel}: ",
    ranking = "Dal pi\u00f9 alto al pi\u00f9 basso: {items}.",
    structure = "{type} di {value}{by}{groups}{panels}.",
    by = " per {cat}",
    groups = ", per ogni {group} ({levels})",
    panels = ", in {n} pannelli",
    source = "I valori sono letti dai dati del grafico.",
    missing = "{n} valori mancanti non sono mostrati.", missing_1 = "1 valore mancante non \u00e8 mostrato.",
    unsupported = "Questo tipo di grafico ({geoms}) non \u00e8 supportato per i risultati automatici; descrivere a mano l'andamento principale.",
    type_bar = "Grafico a barre", type_point = "Grafico a punti", type_interval = "Grafico di stime con intervalli",
    type_line = "Grafico a linee", type_sf = "Mappa", type_other = "Grafico",
    value = "valore", and = "e", series_sep = ": ", categories = "voci"
  )
)

MONTHS <- list(
  en = month.name,
  it = c("gennaio", "febbraio", "marzo", "aprile", "maggio", "giugno", "luglio", "agosto",
         "settembre", "ottobre", "novembre", "dicembre")
)

tpl <- function(lang, key) {
  t <- TEMPLATES[[lang]][[key]]
  if (is.null(t)) rlang::abort(paste("Missing template:", key), class = "ggaltdata_error")
  t
}

fill_tpl <- function(t, ...) {
  v <- list(...)
  # a label that already ends with a parenthesis ("average score (5 = worst)") would be
  # followed by a second one: write "{what}: value" instead of "{what} (value)"
  if (!is.null(v$what) && grepl(")$", v$what)) {
    t <- gsub("\\{what\\} \\(\\{(\\w+)\\}\\)", "{what}: {\\1}", t)
  }
  for (n in names(v)) {
    val <- as.character(v[[n]])
    # labels that end with punctuation ("Prices are high.") would break the sentence
    # (an ellipsis marks a label shortened by the author and is kept)
    if (!grepl("^[-+]?[0-9]", val) && !grepl("(\\.\\.\\.|\\u2026)$", val)) val <- sub("[.;:]+$", "", val)
    t <- gsub(paste0("{", n, "}"), val, t, fixed = TRUE)
  }
  t
}

choose_digits <- function(v) {
  v <- v[is.finite(v)]
  if (!length(v)) return(0)
  if (all(abs(v - round(v)) < 1e-9)) return(0)
  m <- max(abs(v))
  if (m >= 100) 0 else if (m >= 10) 1 else 2
}

fmt_num <- function(x, lang, digits, percent = FALSE, sign = FALSE) {
  if (length(x) != 1 || is.na(x)) return("NA")
  if (percent) x <- x * 100
  s <- formatC(abs(x), format = "f", digits = digits, big.mark = if (lang == "it") "." else ",",
               decimal.mark = if (lang == "it") "," else ".")
  s <- paste0(if (x < 0) "-" else if (sign && x > 0) "+" else "", s)
  if (percent) paste0(s, "%") else s
}

fmt_x <- function(x, is_date, lang) {
  if (!isTRUE(is_date)) return(fmt_num(x, lang, choose_digits(x)))
  d <- as.Date(x, origin = "1970-01-01")
  if (as.integer(format(d, "%d")) == 1) {
    paste(MONTHS[[lang]][as.integer(format(d, "%m"))], format(d, "%Y"))
  } else {
    format(d, "%Y-%m-%d")
  }
}

join_list <- function(x, lang) {
  if (length(x) <= 1) return(paste(x, collapse = ""))
  paste(paste(utils::head(x, -1), collapse = ", "), tpl(lang, "and"), utils::tail(x, 1))
}

# one fact -> one sentence (short form)
fact_sentence <- function(f, r, lang, digits) {
  pct <- r$percent
  n <- function(x, sign = FALSE) fmt_num(x, lang, digits, pct, sign)
  what <- r$value_label %||% tpl(lang, "value")
  # inside a sentence, "Difference in score" reads "difference in score"; acronyms ("GDP") are kept
  if (grepl("^[A-Z][a-z]", what)) substr(what, 1, 1) <- tolower(substr(what, 1, 1))
  cats <- if (!is.null(r$cat_label) && grepl("s$", r$cat_label)) r$cat_label else tpl(lang, "categories")
  # a single series is named by its variable ("The 10-year yield rises..."); several by their group
  series <- function(s) if (identical(s, "")) paste0(what, " ") else paste0(s, tpl(lang, "series_sep"))
  switch(f$type,
    extremes = if (identical(f$what, "total")) {
      fill_tpl(tpl(lang, "extremes_total"), hi = f$hi, hi_v = n(f$hi_v), lo = f$lo, lo_v = n(f$lo_v))
    } else {
      fill_tpl(tpl(lang, "extremes"), hi = f$hi, hi_v = n(f$hi_v), lo = f$lo, lo_v = n(f$lo_v), what = what)
    },
    largest_part = fill_tpl(tpl(lang, "largest_part"), group = f$group, share = fmt_num(f$share, lang, 0, TRUE)),
    group_gap = fill_tpl(tpl(lang, if (f$all_same) "group_gap_all" else "group_gap_some"),
      hi_g = f$hi_g, lo_g = f$lo_g, k = f$k, n = f$n, cats = cats, cat_max = f$cat_max, gap_max = n(f$gap_max, TRUE)),
    group_means = fill_tpl(tpl(lang, "group_means"), hi_g = f$hi_g, hi_v = n(f$hi_v), lo_g = f$lo_g, lo_v = n(f$lo_v)),
    sign = fill_tpl(tpl(lang, "sign"), k = f$k, n = f$n),
    all_negative = fill_tpl(tpl(lang, "all_negative"), n = f$n),
    all_positive = fill_tpl(tpl(lang, "all_positive"), n = f$n),
    extremes_negative = fill_tpl(tpl(lang, "extremes_negative"), hi = f$hi, hi_v = n(f$hi_v), lo = f$lo,
      lo_v = n(f$lo_v), what = what),
    interval_extremes = fill_tpl(tpl(lang, if (f$overlap) "interval_overlap" else "interval_apart"),
      hi = f$hi, hi_v = n(f$hi_v), hi_l = n(f$hi_l), hi_u = n(f$hi_u),
      lo = f$lo, lo_v = n(f$lo_v), lo_l = n(f$lo_l), lo_u = n(f$lo_u)),
    interval_ref = fill_tpl(tpl(lang, "interval_ref"), k = f$k, n = f$n, ref = n(f$ref), above = f$above, below = f$below),
    trend = {
      s <- fill_tpl(tpl(lang, paste0("trend_", f$direction)), series = series(f$series),
        v0 = n(f$v0), v1 = n(f$v1), x0 = fmt_x(f$x0, f$is_date, lang), x1 = fmt_x(f$x1, f$is_date, lang))
      substr(s, 1, 1) <- toupper(substr(s, 1, 1))
      s
    },
    series_end = fill_tpl(tpl(lang, "series_end"), hi = f$hi, hi_v = n(f$hi_v), lo = f$lo, lo_v = n(f$lo_v)),
    area_extremes = fill_tpl(tpl(lang, "area_extremes"), hi = f$hi, hi_v = n(f$hi_v), lo = f$lo,
      lo_v = n(f$lo_v), median = n(f$median), n = f$n, what = r$labels$fill %||% what),
    "")
}

# extra sentences that only the long description carries
fact_long_extra <- function(f, r, lang, digits) {
  pct <- r$percent
  n <- function(x) fmt_num(x, lang, digits, pct)
  out <- character()
  if (f$type == "trend" && isTRUE(f$peak_inside)) {
    s <- fill_tpl(tpl(lang, "peak"), series = if (identical(f$series, "")) "" else paste0(f$series, ": "),
      peak_v = n(f$peak_v), peak_x = fmt_x(f$peak_x, f$is_date, lang))
    substr(s, 1, 1) <- toupper(substr(s, 1, 1))
    out <- c(out, s)
  }
  if (!is.null(f$ranking) && nrow(f$ranking) > 2) {
    nm <- f$ranking[[1]]
    items <- paste0(nm, " (", vapply(f$ranking$value, n, ""), ")")
    out <- c(out, fill_tpl(tpl(lang, "ranking"), items = paste(items, collapse = "; ")))
  }
  out
}

chart_type <- function(r, lang) {
  d <- primary_layer(r)
  k <- if (is.null(d)) "other" else d$kind[1]
  tpl(lang, paste0("type_", if (k %in% c("bar", "point", "interval", "line", "sf")) k else "other"))
}

structure_sentence <- function(r, lang) {
  d <- primary_layer(r)
  groups <- if (!is.null(d)) unique(stats::na.omit(d$group)) else character()
  fill_tpl(tpl(lang, "structure"),
    type = chart_type(r, lang),
    value = r$value_label %||% tpl(lang, "value"),
    by = if (!is.null(r$cat_label)) fill_tpl(tpl(lang, "by"), cat = r$cat_label) else
      if (!is.null(d) && d$kind[1] == "line" && !is.null(r$x_label)) fill_tpl(tpl(lang, "by"), cat = r$x_label) else "",
    groups = if (length(groups) > 1 && length(groups) <= 8)
      fill_tpl(tpl(lang, "groups"), group = r$group_label %||% "group", levels = join_list(groups, lang)) else "",
    panels = if (r$n_panels > 1) fill_tpl(tpl(lang, "panels"), n = r$n_panels) else "")
}
