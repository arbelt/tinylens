
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tinylens

<!-- badges: start -->

[![R-CMD-check](https://github.com/arbelt/tinylens/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/arbelt/tinylens/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of tinylens is to provide a minimal set of tools for working
with lenses in R.

As of the initial release, this is basically a re-implementation of the
`lenses` package (<https://github.com/cfhammill/lenses>).

## Installation

You can install the development version of tinylens from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("arbelt/tinylens")
```

## Example

The main benefit of lenses is that they are composable, and so allow for
more streamlined transformations of nested data structures.

``` r
library(tinylens)
## basic example code
```
