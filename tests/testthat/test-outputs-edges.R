library(ggplot2)

bars <- data.frame(item = c("Apples", "Pears", "Plums"), score = c(3.61, 2.74, 3.18))
p <- ggplot(bars, aes(item, score)) + geom_col() + labs(y = "average score")

test_that("add_alt keeps a hand-written text unless overwrite = TRUE", {
  mine <- p + labs(alt = "mine")
  expect_identical(get_alt_text(add_alt(mine)), "mine")
  expect_match(get_alt_text(add_alt(mine, overwrite = TRUE)), "Apples has the highest")
  expect_match(get_alt_text(add_alt(p)), "Apples has the highest")
})

test_that("alt_data returns the plotted values with readable names", {
  tb <- alt_data(p)
  expect_named(tb, c("item", "average score"))
  expect_equal(sort(tb[["average score"]]), sort(bars$score))
})

test_that("save_alt writes the text and the CSV equal to alt_data", {
  dir <- withr::local_tempdir()
  f <- save_alt(p, file.path(dir, "fig1.png"))
  expect_true(all(file.exists(f)))
  csv <- utils::read.csv(f[2], check.names = FALSE)
  expect_equal(csv[["average score"]], alt_data(p)[["average score"]])
  expect_match(readLines(f[1])[1], "Apples has the highest")
})

test_that("as_markdown gives a pipe table", {
  md <- as_markdown(p, caption = "Table 1")
  expect_match(md, "^Table 1")
  expect_match(md, "\\| item \\| average score \\|")
  expect_match(md, "\\|:---\\|---:\\|")
})

test_that("as_docx_table adds a table to a Word document", {
  skip_if_not_installed("officer")
  doc <- as_docx_table(p, officer::read_docx(), caption = "Data of Figure 1")
  s <- officer::docx_summary(doc)
  expect_true(any(s$content_type == "table cell" & s$text == "Apples"))
})

test_that("unsupported chart: structure and a classed warning, no error", {
  q <- ggplot(mtcars, aes(factor(cyl), mpg)) + geom_boxplot()
  expect_warning(a <- alt_text(q), class = "ggaltdata_unsupported")
  expect_type(a, "character")
  expect_warning(alt_text(ggplot(faithful, aes(eruptions)) + geom_density()), class = "ggaltdata_unsupported")
})

test_that("many categories: short text within max_chars, top and bottom named", {
  set.seed(1)
  d <- data.frame(state = paste("State", sprintf("%02d", 1:50)), v = round(stats::runif(50, 1, 5), 2))
  a <- alt_text(ggplot(d, aes(state, v)) + geom_col())
  expect_lte(nchar(a), 250)
  expect_match(a, d$state[which.max(d$v)])
  expect_match(alt_describe(ggplot(d, aes(state, v)) + geom_col())$long, "From highest to lowest")
})

test_that("tiny max_chars: truncated with a warning", {
  expect_warning(a <- alt_text(p, max_chars = 20), class = "ggaltdata_truncated")
  expect_lte(nchar(a), 20)
})

test_that("missing values are counted in the long description", {
  d <- rbind(bars, data.frame(item = "Lemons", score = NA))
  x <- suppressWarnings(alt_describe(ggplot(d, aes(item, score)) + geom_col()))
  expect_match(x$long, "missing value")
})

test_that("not a ggplot, or no layers: informative errors", {
  expect_error(alt_text(1), class = "ggaltdata_error")
  expect_error(alt_text(ggplot(bars)), class = "ggaltdata_error")
  expect_error(alt_text(p, lang = "fr"), class = "ggaltdata_error")
})

test_that("describe prints and returns invisibly", {
  out <- capture.output(x <- alt_describe(p))
  expect_s3_class(x, "ggaltdata")
  expect_true(any(grepl("Apples", out)))
})
