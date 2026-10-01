# Contributing to ggaltdata

Thank you for your interest in ggaltdata. This page explains how to report a problem, how to ask for
help and how to propose a change.

## Report a problem

Open an issue at <https://github.com/fniniv/ggaltdata/issues>. A useful report contains:

- a small example that reproduces the problem, made with the
  [reprex](https://reprex.tidyverse.org) package if possible;
- the text that ggaltdata wrote, and the text you expected;
- the versions of R, ggplot2 and ggaltdata (`sessionInfo()` prints all of them).

A wrong number in a text is the most serious kind of problem, because the package promises that every
number comes from the chart. Please say so in the title.

## Ask for help

Open an issue and start its title with "Question:". Questions about a chart type that the package
does not support are welcome too: they show which chart to support next.

## Propose a change

Small fixes, such as a typo in the documentation, can go straight to a pull request. For a larger
change, such as a new chart type, a new finding or a new language, please open an issue first, so
that we can agree on the sentences the package should write before you write the code.

A pull request should:

1. add a test in `tests/testthat/` that fails without the change and passes with it;
2. pass `devtools::test()` and `devtools::check()`;
3. add a line to `NEWS.md`.

CRAN asks for ASCII-only code. If you edit the Italian templates in `R/`, run
`python dev/ascii_escape.py` to turn the accented letters into `\u` escapes.

## Code of conduct

Please note that ggaltdata is released with a [Contributor Code of Conduct](../CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.
