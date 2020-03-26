#' @title Convert a decision tree into a data table of partition coordinates
#'
#' @description Extracts the terminal leaf nodes of a decision tree with one or
#'   two predictor variables. These leaf nodes are then converted into a data
#'   table, where each row represents a partition that can easily be plotted in
#'   coordinate space.
#' @param tree An `rpart` object, or an `rpart`-compatible
#'   object (e.g. produced using `parsnip`).
#' @details This function can be used with a regression or classification tree
#'   containing one or (at most) two continuous predictors.
#' @seealso \code{\link[rpart]{rpart}}.
#' @keywords partition tree, decision tree
#' @return A data table comprising seven columns: the leaf node, its path, a set
#'   of coordinates understandable to `ggplot2` (i.e. xmin, xmax, ymin, ymax),
#'   and a final column corresponding to the predicted value for that leaf.
#' @importFrom data.table :=
#' @export
#' @examples
#' library(rpart)
#' parttree(rpart(Species ~ Sepal.Width + Petal.Width, data=iris))
parttree =
  function(tree) {
    if (!(inherits(tree, "rpart") || inherits(tree, "_rpart"))) {
      stop("This function only accepts rpart objects.\n",
           "The object that you provided is of class type: ", class(tree)[1])
    }

    if (inherits(tree, "_rpart")) {
      y_var = tree$preproc$y_var
      tree = tree$fit
    } else {
      y_var = paste0(tree$call$formula[2])
    }

    nodes = rownames(tree$frame[tree$frame$var == "<leaf>", ])
    yvals = tree$frame[tree$frame$var == "<leaf>", ]$yval
    yvals = attr(tree, "ylevels")[yvals] ## get factor equivalents
    vars = unique(as.character(tree$frame[tree$frame$var != "<leaf>", ]$var))

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
    part_coords =
      part_dt[, `:=`(split = as.double(split))][
        , `:=`(xvar = var == ..vars[1], yvar = var == ..vars[2])][
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

    part_coords$yvals = as.factor(part_coords$yvals)
    colnames(part_coords) = gsub("yvals", y_var, colnames(part_coords))

    return(part_coords)
  }
