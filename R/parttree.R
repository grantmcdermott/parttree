#' @title Convert a decision tree into a data frame of partition coordinates
#'
#' @description Extracts the terminal leaf nodes of a decision tree with one or
#'   two predictor variables. These leaf nodes are then converted into a data
#'   frame, where each row represents a partition that can easily be plotted in
#'   coordinate space.
#' @param tree An \code{\link[rpart]{rpart.object}}, or an object of compatible
#'   type (e.g. a decision tree constructed via the `parsnip` or `mlr3`
#'   front-ends).
#' @param keep_as_dt Logical. The function relies on `data.table` for internal
#'   data manipulation. But it will coerce the final return object into a
#'   regular data frame (default behaviour) unless the user specifies `TRUE`.
#' @param flipaxes Logical. The function will automatically set the yaxis
#'   variable as the first split variable in the tree provided unless
#'   the user specifies `TRUE`.
#' @details This function can be used with a regression or classification tree
#'   containing one or (at most) two continuous predictors.
#' @seealso \code{\link{geom_parttree()}}, \code{\link[rpart]{rpart}}.
#' @return A data frame comprising seven columns: the leaf node, its path, a set
#'   of coordinates understandable to `ggplot2` (i.e. xmin, xmax, ymin, ymax),
#'   and a final column corresponding to the predicted value for that leaf.
#' @importFrom data.table :=
#' @export
#' @examples
#' library(rpart)
#' parttree(rpart(Species ~ Petal.Length + Petal.Width, data=iris))
parttree =
  function(tree, keep_as_dt = FALSE, flipaxes = FALSE) {
    if (!(inherits(tree, "rpart") || inherits(tree, "_rpart") ||
          inherits(tree, "LearnerClassifRpart") || inherits(tree, "LearnerRegrRpart"))) {
      stop("The parttree() function only accepts rpart objects.\n",
           "The object that you provided is of class type: ", class(tree)[1])
    }

    ## parsnip front-end
    if (inherits(tree, "_rpart")) {
      if (is.null(tree$fit)) {
        stop("No model detected.\n",
             "Did you forget to fit a model? See `?parsnip::fit`.")
      }
      tree = tree$fit
    }

    ## mlr3 front-end
    if (inherits(tree, "LearnerClassifRpart") || inherits(tree, "LearnerRegrRpart")) {
      if (is.null(tree$model)) {
        stop("No model detected.\n",
             "Did you forget to assign a learner? See `?mlr3::lrn`.")
      }
      tree = tree$model
    }

    if (nrow(tree$frame)<=1) {
      stop("Cannot plot single node tree.")
    }

    vars = unique(as.character(tree$frame[tree$frame$var != "<leaf>", ]$var))
    if (length(vars)>2) {
      stop("Tree can only have one or two predictors.")
    }

    nodes = rownames(tree$frame[tree$frame$var == "<leaf>", ])

    ## Get details about y variable for later
    ### y variable string (i.e. name)
    y_var = attr(tree$terms, "variables")[[2]]
    ### y values
    yvals = tree$frame[tree$frame$var == "<leaf>", ]$yval
    y_factored = attr(tree$terms, "dataClasses")[paste(y_var)] == "factor"
    ## factor equivalents (if factor)
    if (y_factored) {
      yvals = attr(tree, "ylevels")[yvals]
    }

    part_list =
      lapply(
        nodes,
        function(n) {
          pv = rpart::path.rpart(tree, node=n, print.it = FALSE)
          node = as.integer(paste0(names(pv)))
          pv = unlist(pv)

          pd = data.frame(node = rep(node, times = length(pv)-1))

          pv = sapply(2:length(pv), function(i) pv[i])

          # pd$var = gsub("[[:punct:]].+", "", pv) ## Causes problems when punctuation mark in name, so use below
          pd$var = gsub("<.+|<=.+|>.+|>=.+", "", pv)
          # pd$split = gsub(".+[[:punct:]]", "", pv) ## Use below since we want to keep - and . in split values (e.g. -2.5)
          pd$split = as.numeric(gsub(".+[^[:alnum:]\\-\\.\\s]", "", pv))
          pd$side = gsub("\\w|\\.", "", pv)
          pd$yvals = yvals[nodes==node]
          return(pd)
        }
      )
    part_dt = data.table::rbindlist(part_list)

    ## Trim irrelevant parts of tree
    data.table::setorder(part_dt, node)
    part_dt[, path := paste(var, side, split, collapse = " --> "), by = node]
    part_dt = part_dt[,
                      .SD[(grepl(">", side) & split == max(split)) | (grepl("<", side) & split == min(split))],
                      keyby = .(node, var, side)]

    ## Get the coords data frame
    if (flipaxes) {
      var1 = 2
      var2 = 1
    } else {
      var1 = 1
      var2 = 2
    }
    part_coords =
      part_dt[, `:=`(split = as.double(split))][
        , `:=`(xvar = var == ..vars[var1], yvar = var == ..vars[var2])][
          , `:=`(xmin = ifelse(xvar, ifelse(grepl(">", side), split, NA), NA),
                 xmax = ifelse(xvar, ifelse(grepl("<", side), split, NA), NA),
                 ymin = ifelse(yvar, ifelse(grepl(">", side), split, NA), NA),
                 ymax = ifelse(yvar, ifelse(grepl("<", side), split, NA), NA))][
                   , .(xmin = mean(xmin, na.rm = TRUE),
                       xmax = mean(xmax, na.rm = TRUE),
                       ymin = mean(ymin, na.rm = TRUE),
                       ymax = mean(ymax, na.rm = TRUE)),
                   keyby = .(node, yvals, path)][
                     , `:=`(xmin = ifelse(is.na(xmin), -Inf, xmin),
                            xmax = ifelse(is.na(xmax), Inf, xmax),
                            ymin = ifelse(is.na(ymin), -Inf, ymin),
                            ymax = ifelse(is.na(ymax), Inf, ymax))]

    if (y_factored) {
      part_coords$yvals = as.factor(part_coords$yvals)
    }
    colnames(part_coords) = gsub("yvals", y_var, colnames(part_coords))

    if (!keep_as_dt) {
      part_coords = as.data.frame(part_coords)
    }

    return(part_coords)
  }
