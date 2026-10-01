# Public functions ------------------------------------------------------------------------------

check_lang <- function(lang) {
  if (!lang %in% names(TEMPLATES)) {
    rlang::abort(paste0("`lang` must be one of: ", paste(names(TEMPLATES), collapse = ", "), "."),
                 class = "ggaltdata_error")
  }
  lang
}

warn_unsupported <- function(r) {
  bad <- unique(r$geoms[r$kinds %in% c("unsupported", "scatter")])
  if (length(bad) || is.null(primary_layer(r))) {
    rlang::warn(paste0("No automatic findings for this chart type (", paste(r$geoms, collapse = ", "),
                       "): only its structure is described."), class = "ggaltdata_unsupported")
  }
}

value_digits <- function(r) {
  d <- primary_layer(r)
  if (is.null(d)) return(0)
  v <- c(d$value, d$lower, d$upper)
  choose_digits(if (r$percent) v * 100 else v)
}

#' Generate the short alternative text of a ggplot2 chart
#'
#' Reads the data drawn by the chart and writes one or two sentences with its main finding:
#' highest and lowest values, gaps between two groups, trends of series, intervals that overlap or
#' not. Every number comes from the data of the chart.
#'
#' @param p A ggplot object.
#' @param lang Language of the text: `"en"` (default) or `"it"`.
#' @param max_chars Maximum length of the text in characters (default 250).
#' @param n_extremes How many categories to name at the top and bottom when there are many.
#' @param area_var For maps (`geom_sf`), the column with the names of the areas.
#' @return A character string.
#' @examples
#' library(ggplot2)
#' df <- data.frame(item = c("Apples", "Pears", "Plums"), score = c(3.6, 2.7, 3.2))
#' p <- ggplot(df, aes(item, score)) + geom_col() + labs(y = "average score")
#' alt_text(p)
#' alt_text(p, lang = "it")
#' @export
alt_text <- function(p, lang = "en", max_chars = 250, n_extremes = 3, area_var = NULL) {
  lang <- check_lang(lang)
  x <- gad_describe(p, lang, n_extremes, area_var)
  fit_short(x, max_chars)
}

fit_short <- function(x, max_chars) {
  s <- x$short_sentences
  if (!length(s)) s <- x$structure
  out <- s[1]
  for (t in s[-1]) {
    cand <- paste(out, t)
    if (nchar(cand) <= max_chars) out <- cand else break
  }
  if (nchar(out) > max_chars) {
    cut <- substr(out, 1, max_chars - 1)
    cut <- sub("\\s+\\S*$", "", cut)
    out <- paste0(cut, "\u2026")
    rlang::warn(paste0("The main finding is longer than max_chars = ", max_chars, "; it was shortened."),
                class = "ggaltdata_truncated")
  }
  out
}

gad_describe <- function(p, lang = "en", n_extremes = 3, area_var = NULL) {
  r <- gad_read(p, area_var)
  warn_unsupported(r)
  facts <- gad_facts(r, n_extremes)
  digits <- value_digits(r)
  multi <- r$n_panels > 1
  prefix <- function(f) if (multi && !is.na(f$panel)) fill_tpl(tpl(lang, "in_panel"), panel = f$panel) else ""
  pri <- vapply(facts, function(f) f$priority, numeric(1))
  ord <- order(pri, seq_along(facts))
  short <- vapply(facts[ord], function(f) paste0(prefix(f), fact_sentence(f, r, lang, digits)), "")
  long <- character()
  for (f in facts) {
    long <- c(long, paste0(prefix(f), fact_sentence(f, r, lang, digits)), fact_long_extra(f, r, lang, digits))
  }
  d <- primary_layer(r)
  n_missing <- if (!is.null(d)) sum(is.na(d$value)) else 0
  structure_s <- structure_sentence(r, lang)
  if (is.null(d) || any(r$kinds %in% c("unsupported", "scatter"))) {
    long <- c(fill_tpl(tpl(lang, "unsupported"), geoms = paste(unique(r$geoms), collapse = ", ")), long)
  }
  head_txt <- c(r$labels$title, r$labels$subtitle)
  head_txt <- head_txt[!vapply(head_txt, is.null, logical(1))]
  long_text <- paste(c(
    if (length(head_txt)) paste0(sub("[.]$", "", unlist(head_txt)), ".") else NULL,
    structure_s, long,
    if (n_missing == 1) tpl(lang, "missing_1") else if (n_missing > 1) fill_tpl(tpl(lang, "missing"), n = n_missing) else NULL,
    if (!is.null(r$labels$caption)) r$labels$caption else NULL,
    tpl(lang, "source")), collapse = " ")
  structure(list(short_sentences = short[nzchar(short)], structure = structure_s, long = long_text,
                 table = gad_table(r), facts = facts, lang = lang), class = "ggaltdata")
}

gad_table <- function(r) {
  d <- primary_layer(r)
  if (is.null(d)) return(data.frame())
  k <- d$kind[1]
  out <- data.frame(row.names = seq_len(nrow(d)))
  if (r$n_panels > 1) out$Panel <- d$panel_label
  if (k == "sf") {
    out$Area <- d$area
  } else if (!is.null(d$category)) {
    out[[r$cat_label %||% "Category"]] <- d$category
  } else if (!is.null(d$x)) {
    out[[r$x_label %||% "x"]] <- if (isTRUE(d$x_is_date[1])) as.Date(d$x, origin = "1970-01-01") else d$x
  }
  if (any(!is.na(d$group))) out[[r$group_label %||% "Group"]] <- d$group
  vname <- if (k == "sf") (r$labels$fill %||% "Value") else (r$value_label %||% "Value")
  out[[vname]] <- d$value
  if (!is.null(d$lower)) {
    out[["Lower bound"]] <- d$lower
    out[["Upper bound"]] <- d$upper
  }
  rownames(out) <- NULL
  out
}

#' Long description and data table, for reading in the R console
#'
#' Prints a plain-text description of the chart (title, structure, every finding, ranking) and its
#' data table, in a form that a screen reader reads line by line. Returns the result invisibly.
#'
#' @inheritParams alt_text
#' @return Invisibly, an object of class `ggaltdata` with elements `short_sentences`, `long`,
#'   `table` and `facts`.
#' @examples
#' library(ggplot2)
#' df <- data.frame(item = c("Apples", "Pears", "Plums"), score = c(3.6, 2.7, 3.2))
#' alt_describe(ggplot(df, aes(item, score)) + geom_col())
#' @export
alt_describe <- function(p, lang = "en", n_extremes = 3, area_var = NULL) {
  x <- gad_describe(p, check_lang(lang), n_extremes, area_var)
  print(x)
  invisible(x)
}

#' @export
print.ggaltdata <- function(x, ...) {
  cat(strwrap(x$long, width = 80), sep = "\n")
  cat("\n")
  if (nrow(x$table)) print(x$table, row.names = FALSE)
  invisible(x)
}

#' The data table of a chart
#'
#' @inheritParams alt_text
#' @return A data frame with the plotted values under readable column names.
#' @examples
#' library(ggplot2)
#' df <- data.frame(item = c("Apples", "Pears"), score = c(3.6, 2.7))
#' alt_data(ggplot(df, aes(item, score)) + geom_col())
#' @export
alt_data <- function(p, area_var = NULL) {
  gad_table(gad_read(p, area_var))
}

#' Store a generated alternative text in the plot
#'
#' Fills `labs(alt = )`, so that `ggplot2::get_alt_text()`, Shiny's `renderPlot()` and tools that
#' read it use the generated text. A text already written by hand is kept unless `overwrite = TRUE`.
#'
#' @inheritParams alt_text
#' @param overwrite Replace an existing alternative text.
#' @return The plot, with the alternative text set.
#' @examples
#' library(ggplot2)
#' df <- data.frame(item = c("Apples", "Pears"), score = c(3.6, 2.7))
#' p <- add_alt(ggplot(df, aes(item, score)) + geom_col())
#' ggplot2::get_alt_text(p)
#' @export
add_alt <- function(p, lang = "en", max_chars = 250, overwrite = FALSE, area_var = NULL) {
  old <- ggplot2::get_alt_text(p)
  if (length(old) && nzchar(old) && !overwrite) return(p)
  p + ggplot2::labs(alt = alt_text(p, lang = lang, max_chars = max_chars, area_var = area_var))
}

as_result <- function(x, lang = "en") {
  if (inherits(x, "ggaltdata")) x else gad_describe(x, check_lang(lang))
}

#' The data table as a Markdown table
#'
#' @param x A ggplot object or the result of [alt_describe()].
#' @param caption Optional caption written above the table.
#' @param digits Digits for numbers; by default chosen from the data.
#' @param lang Language used for numbers (decimal mark).
#' @return A character string with a pipe table.
#' @examples
#' library(ggplot2)
#' df <- data.frame(item = c("Apples", "Pears"), score = c(3.6, 2.7))
#' cat(as_markdown(ggplot(df, aes(item, score)) + geom_col()))
#' @export
as_markdown <- function(x, caption = NULL, digits = NULL, lang = "en") {
  tb <- as_result(x, lang)$table
  if (!nrow(tb)) return("")
  num <- vapply(tb, is.numeric, logical(1))
  dg <- digits %||% choose_digits(unlist(tb[num]))
  cells <- lapply(names(tb), function(n) {
    v <- tb[[n]]
    if (is.numeric(v)) vapply(v, function(z) fmt_num(z, lang, dg), "") else as.character(v)
  })
  head <- paste0("| ", paste(names(tb), collapse = " | "), " |")
  sep <- paste0("|", paste(ifelse(num, "---:", ":---"), collapse = "|"), "|")
  rows <- vapply(seq_len(nrow(tb)), function(i) paste0("| ", paste(vapply(cells, `[`, "", i), collapse = " | "), " |"), "")
  paste(c(if (!is.null(caption)) c(caption, "") else NULL, head, sep, rows), collapse = "\n")
}

#' Add the data table to a Word document
#'
#' Requires the officer package.
#'
#' @param x A ggplot object or the result of [alt_describe()].
#' @param doc An `rdocx` object from `officer::read_docx()`.
#' @param caption Optional caption written before the table.
#' @param lang Language of the generated texts, if `x` is a plot.
#' @return The `rdocx` object with the table added.
#' @export
as_docx_table <- function(x, doc, caption = NULL, lang = "en") {
  if (!requireNamespace("officer", quietly = TRUE)) {
    rlang::abort("Package 'officer' is needed for as_docx_table(); install it with install.packages('officer').",
                 class = "ggaltdata_error")
  }
  tb <- as_result(x, lang)$table
  if (!is.null(caption)) doc <- officer::body_add_par(doc, caption)
  officer::body_add_table(doc, tb, header = TRUE)
}

#' Save the alternative text and the data table next to an image
#'
#' Writes `<file>.alt.txt` (short and long text) and `<file>.data.csv` beside the image saved with
#' `ggplot2::ggsave()`.
#'
#' @inheritParams alt_text
#' @param file Path of the image (for example `"figure1.png"`).
#' @return Invisibly, the paths of the two files written.
#' @export
save_alt <- function(p, file, lang = "en", max_chars = 250, area_var = NULL) {
  x <- gad_describe(p, check_lang(lang), area_var = area_var)
  base <- sub("\\.[^.]+$", "", file)
  txt <- paste0(base, ".alt.txt")
  csv <- paste0(base, ".data.csv")
  writeLines(c(fit_short(x, max_chars), "", x$long), txt, useBytes = TRUE)
  utils::write.csv(x$table, csv, row.names = FALSE, fileEncoding = "UTF-8")
  invisible(c(txt, csv))
}
