#' @title Visualise tree partitions
#'
#' @description `geom_parttree()` is a simple extension of
#'   [ggplot2::geom_rect()]that first calls
#'   [parttree()] to convert the inputted tree object into an
#'   amenable data frame.
#' @param data An [rpart::rpart.object] or an object of compatible
#'   type (e.g. a decision tree constructed via the `partykit`, `tidymodels`, or
#'   `mlr3` front-ends).
#' @param flipaxes Logical. By default, the "x" and "y" axes variables for
#'   plotting are determined by the first split in the tree. This can cause
#'   plot orientation mismatches depending on how users specify the other layers
#'   of their plot. Setting to `TRUE` will flip the "x" and "y" variables for
#'   the `geom_parttree` layer.
#' @import ggplot2
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::geom_segment
#' @details Because of the way that `ggplot2` validates inputs and assembles
#'   plot layers, note that the data input for `geom_parttree()` (i.e. decision
#'   tree object) must assigned in the layer itself; not in the initialising
#'   [ggplot2::ggplot()] call. See Examples.
#' @section Aesthetics:
#' \code{geom_parttree()} aims to "work-out-of-the-box" with minimal input from
#' the user's side, apart from specifying the data object. This includes taking
#' care of the data transformation in a way that, generally, produces optimal
#' corner coordinates for each partition (i.e. `xmin`, `xmax`, `ymin`, and
#' `ymax`). However, it also understands the following aesthetics that users
#' may choose to specify manually:
#' \itemize{
#'  \item{\code{fill} (particularly encouraged, since this will provide a visual
#'  cue regarding the prediction in each partition region)}
#'  \item{\code{colour}}
#'  \item{\code{alpha}}
#'  \item{\code{linetype}}
#'  \item{\code{size}}
#' }
#'
#' @seealso [parttree()], [ggplot2::geom_rect()].
#' @export
#' @examples
#' library(rpart)
#'
#' ### Simple decision tree (max of two predictor variables)
#'
#' iris_tree = rpart(Species ~ Petal.Length + Petal.Width, data=iris)
#'
#' ## Plot with original iris data only
#' p = ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width)) +
#'   geom_point(aes(col = Species))
#'
#' ## Add tree partitions to the plot (borders only)
#' p + geom_parttree(data = iris_tree)
#'
#' ## Better to use fill and highlight predictions
#' p + geom_parttree(data = iris_tree, aes(fill = Species), alpha=0.1)
#'
#' ## To drop the black border lines (i.e. fill only)
#' p + geom_parttree(data = iris_tree, aes(fill = Species), col = NA, alpha = 0.1)
#'
#'
#' ### Example with plot orientation mismatch
#'
#' p2 = ggplot(iris, aes(x=Petal.Width, y=Petal.Length)) +
#'   geom_point(aes(col=Species))
#'
#' ## Oops
#' p2 + geom_parttree(data = iris_tree, aes(fill=Species), alpha = 0.1)
#'
#' ## Fix with 'flipaxes = TRUE'
#' p2 + geom_parttree(data = iris_tree, aes(fill=Species), alpha = 0.1, flipaxes = TRUE)
#'
#'
#' ### Various front-end frameworks are also supported, e.g.:
#'
#' library(parsnip)
#'
#' iris_tree_parsnip =
#'   decision_tree() %>%
#'   set_engine("rpart") %>%
#'   set_mode("classification") %>%
#'   fit(Species ~ Petal.Length + Petal.Width, data=iris)
#'
#' p + geom_parttree(data = iris_tree_parsnip, aes(fill=Species), alpha = 0.1)
#'
#'
#' ### Trees with continuous independent variables are also supported. But you
#' ### may need to adjust (or switch off) the fill legend to match the original
#' ### data, e.g.:
#'
#' iris_tree_cont = rpart(Petal.Length ~ Sepal.Length + Petal.Width, data=iris)
#' p3 = ggplot(data = iris, aes(x = Petal.Width, y = Sepal.Length)) +
#'  geom_parttree(
#'    data = iris_tree_cont,
#'    aes(fill = Petal.Length), alpha=0.5
#'    ) +
#'   geom_point(aes(col = Petal.Length)) +
#'   theme_minimal()
#'
#' ## Legend scales don't quite match here:
#' p3
#'
#' ## Better to scale fill to the original data
#' p3 + scale_fill_continuous(limits = range(iris$Petal.Length))
geom_parttree =
  function(mapping = NULL, data = NULL,
           stat = "identity", position = "identity",
           linejoin = "mitre", na.rm = FALSE, show.legend = NA,
           inherit.aes = TRUE, flipaxes = FALSE, ...) {
    pdata = parttree(data, flipaxes = flipaxes)
    mapping_null = is.null(mapping)
    mapping$xmin = quote(xmin)
    mapping$xmax = quote(xmax)
    mapping$ymin = quote(ymin)
    mapping$ymax = quote(ymax)
    if (mapping_null) {
      mapping = aes_all(mapping)
    }
    mapping$x = rlang::quo(NULL)
    mapping$y = rlang::quo(NULL)
    layer(
      stat = stat, geom = GeomParttree,
      data = pdata,
      mapping = mapping,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )
  }

## Underlying ggproto object
GeomParttree =
  ggproto(
    "GeomParttree", GeomRect,
    default_aes = aes(colour = "black", linewidth = 0.5, linetype = 1,
                      x=NULL, y = NULL,
                      fill = NA, alpha = NA
    ),
    non_missing_aes = c("x", "y", "xmin", "xmax", "ymin", "ymax")
  )
