
<!-- README.md is generated from README.Rmd. Please edit that file -->

# parttree <a href='https://grantmcdermott.com/parttree/'><img src='man/figures/hex.png' align="right" width="120" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/grantmcdermott/parttree/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/grantmcdermott/parttree/actions/workflows/R-CMD-check.yaml)
[![Docs](https://img.shields.io/badge/docs-homepage-blue.svg)](https://grantmcdermott.com/parttree/index.html)
<!-- badges: end -->

Visualize simple 2-D decision tree partitions in R. The **parttree**
package is optimised to work with
[**ggplot2**](https://ggplot2.tidyverse.org/), although it can be used
to visualize tree partitions with base R graphics too.

## Installation

This package is not yet on CRAN, but can be installed from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("grantmcdermott/parttree")
```

## Quickstart

The **parttree**
[homepage](https://grantmcdermott.com/parttree/index.html) includes an
introductory vignette and detailed documentation. But here’s a
quickstart example using the
[“kyphosis”](https://search.r-project.org/CRAN/refmans/rpart/html/kyphosis.html)
dataset that comes bundled with the **rpart** package. In this case, we
are interested in predicting kyphosis recovery after spinal surgery, as
a function of 1) the number of topmost vertebra that were operated, and
2) patient age. The key visualization layer below—provided by this
package—is `geom_partree()`.

``` r
library(rpart)     # For the dataset and fitting decisions trees
library(parttree)  # This package (will automatically load ggplot2 too)
#> Loading required package: ggplot2

fit = rpart(Kyphosis ~ Start + Age, data = kyphosis)

ggplot(kyphosis, aes(x = Start, y = Age)) +
  geom_parttree(data = fit, alpha = 0.1, aes(fill = Kyphosis)) + # <-- key layer
  geom_point(aes(col = Kyphosis)) +
  labs(
    x = "No. of topmost vertebra operated on", y = "Patient age (months)",
    caption = "Note: Points denote observations. Shading denotes model predictions."
    ) +
  theme_minimal()
```

<img src="man/figures/README-quickstart-1.png" width="100%" />
