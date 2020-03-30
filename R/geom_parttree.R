#' @title Visualise tree partitions
#'
#' @description \code{geom_parttree()} is a simple extension of
#'   \code{\link[ggplot2]{geom_rect()}} that first calls \code{\link{parttree()}}
#'   to convert the inputted tree object into an amenable data frame.
#' @param data An \code{\link[rpart]{rpart.object}}, or an object of compatible
#'   type (e.g. a decision tree constructed via the `parsnip` or `mlr3`
#'   front-ends).
#' @import ggplot2
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::geom_segment
#' @details Note that, because of the way \code{ggplot2} validates inputs and
#'   assembles plot layers, the data input for \code{geom_parttree()} (i.e. an
#'   \code{\link[rpart]{rpart.object}}) must assigned in the layer itself; not
#'   in the initialising \code{\link[ggplot2]{ggplot2()}} call. See Examples.
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
#' @seealso \code{\link{parttree()}}, \code{\link[ggplot2]{geom_rect()}}.
#' @export
#' @examples
#' library(ggplot2)
#' library(rpart)
#'
#' ## Simple decision tree (max of two predictor variables)
#' iris_tree = rpart(Species ~ Sepal.Width + Petal.Width, data=iris)

#' ## Plot with original iris data only
#' p = ggplot(data = iris, aes(x=Petal.Width, y=Sepal.Width)) +
#'   geom_point(aes(col=Species)) ## Original data

#' ## Add tree partitions to the plot (borders only)
#' p + geom_parttree(data = iris_tree)

#' ## Better to use fill highlight predictions
#' p + geom_parttree(data = iris_tree, aes(fill=Species), alpha = 0.1)
#'
#' ## Various front-end frameworks are also supported, e.g.:
#' library(parsnip)
#'
#' iris_tree2 =
#'   decision_tree() %>%
#'   set_engine("rpart") %>%
#'   set_mode("classification") %>%
#'   fit(Species ~ Sepal.Width + Petal.Width, data=iris)
#'
#' p + geom_parttree(data = iris_tree2, aes(fill=Species), alpha = 0.1)
geom_parttree <-
  function(mapping = NULL, data = NULL,
           stat = "identity", position = "identity",
           linejoin = "mitre", na.rm = FALSE, show.legend = NA,
           inherit.aes = TRUE, ...) {
    pdata <- parttree(data)
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
GeomParttree <-
  ggproto(
    "GeomParttree", GeomRect,
    default_aes = aes(colour = "black", size = 0.5, linetype = 1,
                      x=NULL, y = NULL,
                      fill = NA, alpha = NA
    ),
    non_missing_aes = c("x", "y", "xmin", "xmax", "ymin", "ymax")
  )
