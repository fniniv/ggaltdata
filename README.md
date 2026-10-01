# ggaltdata

Alternative text that says what a ggplot2 chart shows, written from the data of the chart.

Accessibility rules (WCAG 2.1 success criterion 1.1.1, the ADA Title II rule in the United States,
EN 301 549 and the European Accessibility Act in Europe) ask for a text alternative for every chart
that carries information. ggaltdata reads the values a chart draws and writes:

- a **short alternative text**, one or two sentences with the main finding;
- a **long description**, with every finding and the ranking, readable by a screen reader;
- the **data table** of the chart, for an appendix or a download.

Every number in the text is a number drawn by the chart: there is no language model and nothing is
estimated. English and Italian.

## Installation

```r
# development version
# install.packages("remotes")
remotes::install_github("fniniv/ggaltdata")
```

## Example

```r
library(ggplot2)
library(ggaltdata)

scores <- data.frame(
  item = rep(c("Apples", "Pears", "Plums", "Figs"), 2),
  area = rep(c("North", "South"), each = 4),
  score = c(2.80, 3.05, 3.35, 2.95, 3.35, 3.30, 3.50, 3.25)
)
p <- ggplot(scores, aes(item, score, fill = area)) +
  geom_col(position = "dodge") +
  labs(x = "products", y = "average score")

alt_text(p)
#> "South is higher than North on all 4 products; the widest gap is Apples (+0.55)."

alt_text(p, lang = "it")
#> "South è più alto di North in tutte le 4 voci; la distanza maggiore è in Apples (+0,55)."

alt_describe(p)      # long description and data table in the console
alt_data(p)      # the data table as a data frame
p <- add_alt(p)  # stores the text in labs(alt = ), read by get_alt_text() and Shiny
```

## Supported charts

Bars and dots (one series, two or more groups, stacked), differences around zero (lollipop),
estimates with intervals and reference lines, lines over time, maps with `geom_sf`, and facets. Other
chart types get a description of their structure and a warning.

## Where the text goes

R Markdown (`fig.alt = alt_text(p)`), Quarto (`#| fig-alt: !expr alt_text(p)`), Shiny
(`renderPlot(p, alt = alt_text(p))`), saved images (`save_alt(p, "figure.png")`), Word with officer
(`as_docx_table()`), Markdown tables (`as_markdown()`). See `vignette("ggaltdata")`.

## Related work

ggplot2's `labs(alt = )` and `get_alt_text()` store a text written by hand. ggalttext generates a text
from the structure of a plot (layers, axes, labels). BrailleR describes plots to blind R users, and
maidr makes charts explorable by keyboard and sound. MatplotAlt does data-based alt text for
matplotlib in Python. ggaltdata adds data-based findings and the data table for ggplot2.

## Getting help and contributing

To report a problem, ask a question or propose a change, see
[CONTRIBUTING](https://github.com/fniniv/ggaltdata/blob/main/.github/CONTRIBUTING.md). Issues go to <https://github.com/fniniv/ggaltdata/issues>.

## Code of Conduct

Please note that the ggaltdata project is released with a
[Contributor Code of Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## License

MIT © Federico Ninivaggi
