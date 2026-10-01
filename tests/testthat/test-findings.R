library(ggplot2)

# every number written in a text must be one of the allowed numbers (data, rounded as written)
numbers_in <- function(txt) {
  m <- regmatches(txt, gregexpr("[-+]?[0-9]+(?:[.,][0-9]+)?", txt, perl = TRUE))[[1]]
  as.numeric(sub(",", ".", sub("^\\+", "", m)))
}
expect_numbers_from <- function(txt, allowed, digits = 2) {
  got <- numbers_in(txt)
  ok <- vapply(got, function(g) any(abs(round(allowed, digits) - g) < 1e-9), logical(1))
  expect_true(all(ok), info = paste("not in data:", paste(got[!ok], collapse = ", "), "|", txt))
}

bars <- data.frame(item = c("Apples", "Pears", "Plums", "Figs", "Kiwis", "Limes"),
                   score = c(3.61, 2.74, 3.18, 3.47, 3.02, 3.95))

test_that("single bars: highest and lowest with their values", {
  p <- ggplot(bars, aes(item, score)) + geom_col() + labs(y = "average score")
  a <- alt_text(p)
  expect_match(a, "Limes has the highest average score \\(3.95\\)")
  expect_match(a, "Pears the lowest \\(2.74\\)")
  expect_numbers_from(a, bars$score)
  expect_lte(nchar(a), 250)
})

test_that("horizontal bars give the same facts as vertical ones", {
  v <- alt_text(ggplot(bars, aes(item, score)) + geom_col() + labs(y = "average score"))
  h <- alt_text(ggplot(bars, aes(score, item)) + geom_col() + labs(x = "average score"))
  expect_identical(v, h)
})

grouped <- data.frame(item = rep(bars$item, 2), area = rep(c("North", "South"), each = 6),
                      score = c(2.80, 3.05, 3.35, 2.95, 3.20, 3.70, 3.35, 3.30, 3.50, 3.25, 3.42, 3.81))

test_that("two groups: direction on all items and widest gap", {
  p <- ggplot(grouped, aes(item, score, fill = area)) + geom_col(position = "dodge")
  a <- alt_text(p)
  expect_match(a, "South is higher than North on all 6 categories")
  gaps <- grouped$score[7:12] - grouped$score[1:6]
  expect_match(a, paste0("\\(\\+", format(round(max(gaps), 2), nsmall = 2), "\\)"))
})

test_that("two groups with exceptions: k of n", {
  g2 <- grouped
  g2$score[7] <- 2.70  # South below North on Apples
  a <- alt_text(ggplot(g2, aes(item, score, fill = area)) + geom_col(position = "dodge"))
  expect_match(a, "on 5 of 6 categories")
})

test_that("stacked bars: totals and largest part", {
  p <- ggplot(grouped, aes(item, score, fill = area)) + geom_col()
  a <- alt_text(p)
  tot <- tapply(grouped$score, grouped$item, sum)
  expect_match(a, paste0(names(which.max(tot)), " has the highest total"))
  sh <- tapply(grouped$score, grouped$area, sum) / sum(grouped$score)
  expect_numbers_from(a, c(tot, round(100 * sh)))
})

test_that("lollipop of differences around zero: sign count", {
  d <- data.frame(item = letters[1:5], gap = c(0.5, 0.3, -0.1, 0.2, 0.4))
  p <- ggplot(d, aes(gap, item)) + geom_segment(aes(x = 0, xend = gap, yend = item)) + geom_point() +
    labs(x = "gap")
  a <- alt_text(p)
  expect_match(a, "a has the highest gap \\(0.50\\)")
  expect_match(alt_describe(p)$long, "4 of 5 values are above zero")
})

towns <- data.frame(town = c("Alder", "Birch", "Cedar", "Maple"), est = c(4.05, 3.90, 3.31, 3.82),
                    lo = c(3.93, 3.77, 3.15, 3.70), hi = c(4.17, 4.03, 3.47, 3.94))

test_that("intervals: extremes, overlap and reference line", {
  p <- ggplot(towns, aes(est, town)) + geom_pointrange(aes(xmin = lo, xmax = hi)) +
    geom_vline(xintercept = 3.85) + labs(x = "satisfaction")
  x <- alt_describe(p)
  expect_match(x$long, "Alder has the highest estimate \\(4.05; 3.93 to 4.17\\)")
  expect_match(x$long, "do not overlap")
  expect_match(x$long, "2 of 4 intervals exclude the reference value 3.85 \\(1 above, 1 below\\)")
  close <- towns
  close$lo[3] <- 3.20; close$hi[3] <- 3.97
  y <- alt_describe(ggplot(close, aes(est, town)) + geom_pointrange(aes(xmin = lo, xmax = hi)))
  expect_match(y$long, "overlap, so the difference is uncertain")
})

ts <- data.frame(month = seq(as.Date("2026-01-01"), by = "month", length.out = 9),
                 y10 = c(4.26, 3.97, 4.30, 4.40, 4.45, 4.44, 4.75, 4.75, 5.26))

test_that("one series: rise with dates", {
  a <- alt_text(ggplot(ts, aes(month, y10)) + geom_line() + labs(y = "10-year yield"))
  expect_match(a, "10-year yield rises from 4.26 in January 2026 to 5.26 in September 2026", ignore.case = TRUE)
})

test_that("flat series within tolerance is stable", {
  flat <- data.frame(t = 1:10, v = c(5, 5.01, 4.99, 5, 5.02, 5, 4.98, 5, 5.01, 5.00))
  a <- alt_text(ggplot(flat, aes(t, v)) + geom_line(), max_chars = 400)
  expect_match(a, "stays around")
  flat2 <- rbind(flat, data.frame(t = 11:12, v = c(9, 9)))
  expect_match(alt_text(ggplot(flat2, aes(t, v)) + geom_line()), "rises")
})

test_that("two series: who ends higher", {
  d <- rbind(data.frame(t = 1:5, v = c(1, 2, 3, 4, 5), s = "A"), data.frame(t = 1:5, v = c(2, 2, 2, 3, 3), s = "B"))
  x <- alt_describe(ggplot(d, aes(t, v, colour = s)) + geom_line())
  expect_match(x$long, "A: rises from 1 in 1 to 5 in 5")
  expect_match(x$long, "At the end, A is highest \\(5\\) and B lowest \\(3\\)")
})

test_that("facets: one fact per panel in the long description", {
  d <- rbind(cbind(bars, region = "West"), cbind(transform(bars, score = rev(score)), region = "East"))
  x <- alt_describe(ggplot(d, aes(item, score)) + geom_col() + facet_wrap(~region))
  expect_match(x$long, "In East: ")
  expect_match(x$long, "In West: ")
})

test_that("Italian: decimal comma and Italian words", {
  a <- alt_text(ggplot(bars, aes(item, score)) + geom_col() + labs(y = "punteggio medio"), lang = "it")
  expect_match(a, "Limes ha il valore più alto di punteggio medio \\(3,95\\)")
  t <- alt_text(ggplot(ts, aes(month, y10)) + geom_line() + labs(y = "rendimento"), lang = "it")
  expect_match(t, "settembre 2026")
})

test_that("percent axis is written as percent", {
  d <- data.frame(g = c("a", "b", "c"), share = c(0.123, 0.456, 0.211))
  a <- alt_text(ggplot(d, aes(g, share)) + geom_col() + scale_y_continuous(labels = function(x) paste0(x * 100, "%")))
  expect_match(a, "45.60%|45.6%")
})

test_that("choropleth: highest and lowest areas", {
  skip_if_not_installed("sf")
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  p <- ggplot(nc) + geom_sf(aes(fill = BIR74)) + labs(fill = "births 1974")
  a <- alt_text(p, area_var = "NAME", max_chars = 400)
  expect_match(a, paste0(nc$NAME[which.max(nc$BIR74)], " has the highest births 1974"))
  expect_match(a, nc$NAME[which.min(nc$BIR74)])
})

test_that("facets with free scales read each panel's own categories (regression, 2026-10-01)", {
  d <- data.frame(panel = rep(c("A", "B"), c(3, 2)), item = c("x1", "x2", "x3", "y1", "y2"),
                  gap = c(0.5, 0.2, 0.1, -0.3, -0.1))
  p <- ggplot(d, aes(gap, item)) + geom_segment(aes(x = 0, xend = gap, yend = item)) + geom_point() +
    facet_wrap(~panel, ncol = 1, scales = "free_y")
  tb <- alt_data(p)
  expect_equal(tb$gap[match(c("x1", "y1"), tb$item)], c(0.5, -0.3))
  x <- alt_describe(p)
  expect_match(x$long, "In A: All 3 values are above zero")
  expect_match(x$long, "In B: All 2 values are below zero")
  expect_match(x$long, "x1 has the highest gap (0.50)", fixed = TRUE)
  expect_match(x$long, "y1 has the largest negative gap (-0.30)", fixed = TRUE)
})

test_that("labels ending with a full stop or a parenthesis read well (2026-10-01)", {
  d <- data.frame(item = c("Prices are high.", "Queues are long."), v = c(3.47, 2.26))
  a <- alt_text(ggplot(d, aes(item, v)) + geom_col() + labs(y = "average score (5 = worst)"))
  expect_false(grepl(". has", a, fixed = TRUE))
  expect_match(a, "average score (5 = worst): 3.47", fixed = TRUE)
})

test_that("a label shortened with an ellipsis keeps it", {
  d <- data.frame(item = c("A very long label that the author shortened by ha...", "Short"), v = c(1, 2))
  a <- alt_text(ggplot(d, aes(item, v)) + geom_col())
  expect_match(a, "by ha... the lowest", fixed = TRUE)
})
