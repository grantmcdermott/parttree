# Visualize tree partitions with ggplot2

`geom_parttree()` is a simple wrapper around
[`parttree()`](https://grantmcdermott.com/parttree/reference/parttree.md)
that takes a tree model object and then converts into an amenable data
frame that `ggplot2` knows how to plot. Please note that `ggplot2` is
not a hard dependency of `parttree` and must thus be installed
separately on the user's system before calling `geom_parttree`.

## Usage

``` r
geom_parttree(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  linejoin = "mitre",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  flip = FALSE,
  ...
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  An
  [rpart::rpart.object](https://rdrr.io/pkg/rpart/man/rpart.object.html)
  or an object of compatible type (e.g. a decision tree constructed via
  the `partykit`, `tidymodels`, or `mlr3` front-ends).

- stat:

  The statistical transformation to use on the data for this layer. When
  using a `geom_*()` function to construct a layer, the `stat` argument
  can be used to override the default coupling between geoms and stats.
  The `stat` argument accepts the following:

  - A `Stat` ggproto subclass, for example `StatCount`.

  - A string naming the stat. To give the stat as a string, strip the
    function name of the `stat_` prefix. For example, to use
    [`stat_count()`](https://ggplot2.tidyverse.org/reference/geom_bar.html),
    give the stat as `"count"`.

  - For more information and other ways to specify the stat, see the
    [layer
    stat](https://ggplot2.tidyverse.org/reference/layer_stats.html)
    documentation.

- position:

  A position adjustment to use on the data for this layer. This can be
  used in various ways, including to prevent overplotting and improving
  the display. The `position` argument accepts the following:

  - The result of calling a position function, such as
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html).
    This method allows for passing extra arguments to the position.

  - A string naming the position adjustment. To give the position as a
    string, strip the function name of the `position_` prefix. For
    example, to use
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html),
    give the position as `"jitter"`.

  - For more information and other ways to specify the position, see the
    [layer
    position](https://ggplot2.tidyverse.org/reference/layer_positions.html)
    documentation.

- linejoin:

  Line join style (round, mitre, bevel).

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are silently removed.

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes. It can also be a named logical
  vector to finely select the aesthetics to display. To include legend
  keys for all levels, even when no data exists, use `TRUE`. If `NA`,
  all levels are shown in legend, but unobserved levels are omitted.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behaviour from the default
  plot specification, e.g.
  [`annotation_borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).

- flip:

  Logical. By default, the "x" and "y" axes variables for plotting are
  determined by the first split in the tree. This can cause plot
  orientation mismatches depending on how users specify the other layers
  of their plot. Setting to `TRUE` will flip the "x" and "y" variables
  for the `geom_parttree` layer.

- ...:

  Other arguments passed on to
  [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html)'s
  `params` argument. These arguments broadly fall into one of 4
  categories below. Notably, further arguments to the `position`
  argument, or aesthetics that are required can *not* be passed through
  `...`. Unknown arguments that are not part of the 4 categories below
  are ignored.

  - Static aesthetics that are not mapped to a scale, but are at a fixed
    value and apply to the layer as a whole. For example,
    `colour = "red"` or `linewidth = 3`. The geom's documentation has an
    **Aesthetics** section that lists the available options. The
    'required' aesthetics cannot be passed on to the `params`. Please
    note that while passing unmapped aesthetics as vectors is
    technically possible, the order and required length is not
    guaranteed to be parallel to the input data.

  - When constructing a layer using a `stat_*()` function, the `...`
    argument can be used to pass on parameters to the `geom` part of the
    layer. An example of this is
    `stat_density(geom = "area", outline.type = "both")`. The geom's
    documentation lists which parameters it can accept.

  - Inversely, when constructing a layer using a `geom_*()` function,
    the `...` argument can be used to pass on parameters to the `stat`
    part of the layer. An example of this is
    `geom_area(stat = "density", adjust = 0.5)`. The stat's
    documentation lists which parameters it can accept.

  - The `key_glyph` argument of
    [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html) may
    also be passed on through `...`. This can be one of the functions
    described as [key
    glyphs](https://ggplot2.tidyverse.org/reference/draw_key.html), to
    change the display of the layer in the legend.

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html) layer.

## Details

Because of the way that `ggplot2` validates inputs and assembles plot
layers, note that the data input for `geom_parttree()` (i.e. decision
tree object) must assigned in the layer itself; not in the initialising
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
call. See Examples.

## Aesthetics

`geom_parttree()` aims to "work-out-of-the-box" with minimal input from
the user's side, apart from specifying the data object. This includes
taking care of the data transformation in a way that, generally,
produces optimal corner coordinates for each partition (i.e. `xmin`,
`xmax`, `ymin`, and `ymax`). However, it also understands the following
aesthetics that users may choose to specify manually:

- `fill` (particularly encouraged, since this will provide a visual cue
  regarding the prediction in each partition region)

- `colour`

- `alpha`

- `linetype`

- `size`

## See also

[`plot.parttree()`](https://grantmcdermott.com/parttree/reference/plot.parttree.md),
which provides an alternative plotting method using base R graphics.

## Examples

``` r
# install.packages("ggplot2")
library(ggplot2)  # ggplot2 must be installed/loaded separately

library(parttree) # this package
library(rpart)    # decision trees
#
## Simple decision tree (max of two predictor variables)

iris_tree = rpart(Species ~ Petal.Length + Petal.Width, data=iris)

# Plot with original iris data only
p = ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(aes(col = Species))

# Add tree partitions to the plot (borders only)
p + geom_parttree(data = iris_tree)


# Better to use fill and highlight predictions
p + geom_parttree(data = iris_tree, aes(fill = Species), alpha=0.1)


# To drop the black border lines (i.e. fill only)
p + geom_parttree(data = iris_tree, aes(fill = Species), col = NA, alpha = 0.1)


#
## Example with plot orientation mismatch

p2 = ggplot(iris, aes(x=Petal.Width, y=Petal.Length)) +
  geom_point(aes(col=Species))

# Oops
p2 + geom_parttree(data = iris_tree, aes(fill=Species), alpha = 0.1)


# Fix with 'flip = TRUE'
p2 + geom_parttree(data = iris_tree, aes(fill=Species), alpha = 0.1, flip = TRUE)


#
## Various front-end frameworks are also supported, e.g.:

# install.packages("parsnip")
library(parsnip)

iris_tree_parsnip = decision_tree() |>
  set_engine("rpart") |>
  set_mode("classification") |>
  fit(Species ~ Petal.Length + Petal.Width, data=iris)

p + geom_parttree(data = iris_tree_parsnip, aes(fill=Species), alpha = 0.1)


#
## Trees with continuous independent variables are also supported.

# Note: you may need to adjust (or switch off) the fill legend to match the
# original data, e.g.:

iris_tree_cont = rpart(Petal.Length ~ Sepal.Length + Petal.Width, data=iris)
p3 = ggplot(data = iris, aes(x = Petal.Width, y = Sepal.Length)) +
 geom_parttree(
   data = iris_tree_cont,
   aes(fill = Petal.Length), alpha=0.5
   ) +
  geom_point(aes(col = Petal.Length)) +
  theme_minimal()

# Legend scales don't quite match here:
p3


# Better to scale fill to the original data
p3 + scale_fill_continuous(limits = range(iris$Petal.Length))

```
