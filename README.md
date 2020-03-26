
<!-- README.md is generated from README.Rmd. Please edit that file -->

# parttree

<!-- badges: start -->

<!-- badges: end -->

A set of simple functions for visualizing decision tree partitions in R.

## Installation

This package is not yet on CRAN, but can be installed from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("grantmcdermott/parttree")
```

## Examples

The main goal of the package is to convert decision tree objects
(e.g. created with
[**rpart**](https://cran.r-project.org/web/packages/rpart/index.html))
into a data frame, which is amenable to visualizing as a partition plot.
Here’s a simple example using everyone’s favourite `iris` dataset.

``` r
library(rpart)
library(parttree)

tree_mod = rpart(Species ~ Sepal.Width + Petal.Width, data=iris)
tree_mod
#> n= 150 
#> 
#> node), split, n, loss, yval, (yprob)
#>       * denotes terminal node
#> 
#> 1) root 150 100 setosa (0.33333333 0.33333333 0.33333333)  
#>   2) Petal.Width< 0.8 50   0 setosa (1.00000000 0.00000000 0.00000000) *
#>   3) Petal.Width>=0.8 100  50 versicolor (0.00000000 0.50000000 0.50000000)  
#>     6) Petal.Width< 1.75 54   5 versicolor (0.00000000 0.90740741 0.09259259) *
#>     7) Petal.Width>=1.75 46   1 virginica (0.00000000 0.02173913 0.97826087) *

## Let's convert it to a partition data frame
tree_mod_parted = parttree(tree_mod)
tree_mod_parted
#>    node    Species                                       path xmin xmax ymin
#> 1:    2     setosa                         Petal.Width <  0.8 -Inf 0.80 -Inf
#> 2:    6 versicolor Petal.Width >= 0.8 --> Petal.Width <  1.75 0.80 1.75 -Inf
#> 3:    7  virginica Petal.Width >= 0.8 --> Petal.Width >= 1.75 1.75  Inf -Inf
#>    ymax
#> 1:  Inf
#> 2:  Inf
#> 3:  Inf
```

Again, the resulting data frame is designed to be compatible with
[**ggplot2**](https://ggplot2.tidyverse.org/) (in particular,
`geom_rect()`). This makes it convenient for visualizing partitions in
conjunction with the original data.

``` r
library(ggplot2)

ggplot() +
  geom_point(data = iris, aes(x=Petal.Width, y=Sepal.Width, col=Species)) +
  geom_rect(
    data = tree_mod_parted, alpha = 0.1, col = "black",
    aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax, fill=Species)
    ) +
  labs(caption = "Note: Points denote observed data. Shaded regions denote tree predictions.")
```

<img src="man/figures/README-iris_plot-1.png" width="100%" />

Currently, the function only works with decision trees created by the
[**rpart**](https://cran.r-project.org/web/packages/rpart/index.html)
package. However, it does support other packages and modes that call
`rpart::rpart()` as the underlying engine. Here’s an example using the
[**parsnip**](https://tidymodels.github.io/parsnip/) package.

``` r
library(titanic) ## Just for a different data set
library(parsnip) ## Will use rpart engine

titanic_train$Survived = as.factor(titanic_train$Survived)

## Build our tree using parsnip (but with rpart as the model engine)
ti_tree =
  decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification") %>%
  fit(Survived ~ Pclass + Age, data = titanic_train)

## Plot the data and model partitions
ggplot() +
  geom_jitter(
    data = titanic_train,
    aes(x=Pclass, y=Age, col=Survived), alpha=0.7
    ) +
  geom_rect(
    data = parttree(ti_tree), alpha = 0.1, col = "black",
    aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax, fill=Survived)
    ) +
  theme_minimal() +
  theme(panel.grid = element_blank())
#> Warning: Removed 177 rows containing missing values (geom_point).
```

<img src="man/figures/README-titanic_plot-1.png" width="100%" />
