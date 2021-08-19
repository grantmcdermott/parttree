#' @title Convert a decision tree into a data frame of partition coordinates
#' @aliases parttree parttree.rpart parttree._rpart parttree.workflow parttree.LearnerClassifRpart parttree.LearnerRegrRpart parttree.constparty
#'
#' @description Extracts the terminal leaf nodes of a decision tree with one or
#'   two numeric predictor variables. These leaf nodes are then converted into a data
#'   frame, where each row represents a partition (or leaf or terminal node)
#'   that can easily be plotted in coordinate space.
#' @param tree A tree object. Supported classes include
#'  [rpart::rpart.object], or the compatible classes from
#'   from the `parsnip`, `workflows`, or `mlr3` front-ends, or the
#'   `constparty` class inheriting from [partykit::party()].
#' @param keep_as_dt Logical. The function relies on `data.table` for internal
#'   data manipulation. But it will coerce the final return object into a
#'   regular data frame (default behavior) unless the user specifies `TRUE`.
#' @param flipaxes Logical. The function will automatically set the y-axis
#'   variable as the first split variable in the tree provided unless
#'   the user specifies `TRUE`.
#' @details This function can be used with a regression or classification tree
#'   containing one or (at most) two numeric predictors.
#' @seealso [geom_parttree()], [rpart::rpart()], [partykit::ctree()].
#' @return A data frame comprising seven columns: the leaf node, its path, a set
#'   of coordinates understandable to `ggplot2` (i.e., xmin, xmax, ymin, ymax),
#'   and a final column corresponding to the predicted value for that leaf.
#' @importFrom data.table :=
#' @importFrom data.table .SD
#' @export
#' @examples
#' ## rpart trees
#' library("rpart")
#' rp = rpart(Species ~ Petal.Length + Petal.Width, data = iris)
#' parttree(rp)
#'
#' ## conditional inference trees
#' library("partykit")
#' ct = ctree(Species ~ Petal.Length + Petal.Width, data = iris)
#' parttree(ct)
#'
#' ## rpart via partykit
#' rp2 = as.party(rp)
#' parttree(rp2)
parttree =
  function(tree, keep_as_dt = FALSE, flipaxes = FALSE) {
    UseMethod("parttree")
  }

#' @export
parttree.rpart =
  function(tree, keep_as_dt = FALSE, flipaxes = FALSE) {
    ## Silence NSE notes in R CMD check. See:
    ## https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html#globals
    node = path = variable = side = ..vars = xvar = yvar = xmin = xmax = ymin = ymax = NULL

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
      ylevs = attr(tree, "ylevels")
      yvals = ylevs[yvals]
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

          # pd$variable = gsub("[[:punct:]].+", "", pv) ## Causes problems when punctuation mark in name, so use below
          pd$variable = gsub("<.+|<=.+|>.+|>=.+", "", pv)
          #pd$split = gsub(".+[[:punct:]]", "", pv) ## Use below since we want to keep - and . in split values (e.g. -2.5)
          pd$split = as.numeric(gsub(".+<|.+<=|>|.+>=", "", pv))
          pd$side = gsub("\\w|\\.", "", pv)
          pd$yvals = yvals[nodes==node]
          return(pd)
        }
      )
    part_dt = data.table::rbindlist(part_list)

    ## Trim irrelevant parts of tree
    data.table::setorder(part_dt, node)
    part_dt[, path := paste(variable, side, split, collapse = " --> "), by = node]
    part_dt = part_dt[,
                      .SD[(grepl(">", side) & split == max(split)) | (grepl("<", side) & split == min(split))],
                      keyby = list(node, variable, side)]

    ## Get the coords data frame
    if (flipaxes) {
      vars = rev(vars)
      ## Handle edge cases with only 1 level
      if (length(vars)==1) {
        missing_var = setdiff(attr(tree$terms, 'term.labels'), vars)
        vars = c(missing_var, vars)
      }
      }
    part_coords =
      part_dt[, `:=`(split = as.double(split))][
        , `:=`(xvar = variable == ..vars[1], yvar = variable == ..vars[2])][
          , `:=`(xmin = ifelse(xvar, ifelse(grepl(">", side), split, NA), NA),
                 xmax = ifelse(xvar, ifelse(grepl("<", side), split, NA), NA),
                 ymin = ifelse(yvar, ifelse(grepl(">", side), split, NA), NA),
                 ymax = ifelse(yvar, ifelse(grepl("<", side), split, NA), NA))][
                   , list(xmin = mean(xmin, na.rm = TRUE),
                          xmax = mean(xmax, na.rm = TRUE),
                          ymin = mean(ymin, na.rm = TRUE),
                          ymax = mean(ymax, na.rm = TRUE)),
                   keyby = list(node, yvals, path)][
                     , `:=`(xmin = ifelse(is.na(xmin), -Inf, xmin),
                            xmax = ifelse(is.na(xmax), Inf, xmax),
                            ymin = ifelse(is.na(ymin), -Inf, ymin),
                            ymax = ifelse(is.na(ymax), Inf, ymax))]

    if (y_factored) {
      part_coords$yvals = factor(part_coords$yvals, levels = ylevs)
    }
    colnames(part_coords) = gsub("yvals", y_var, colnames(part_coords))

    if (!keep_as_dt) {
      part_coords = as.data.frame(part_coords)
    }

    return(part_coords)
  }

#' @export
parttree._rpart =
  function(tree, keep_as_dt = FALSE, flipaxes = FALSE) {
    ## parsnip front-end
    if (is.null(tree$fit)) {
      stop("No model detected.\n",
    	   "Did you forget to fit a model? See `?parsnip::fit`.")
    }
    tree = tree$fit
    parttree.rpart(tree, keep_as_dt = keep_as_dt, flipaxes = flipaxes)
  }

#' @export
parttree.workflow =
  function(tree, keep_as_dt = FALSE, flipaxes = FALSE) {
    ## workflow front-end
    if (!workflows::is_trained_workflow(fitted)) {
      stop("No model detected.\n",
           "Did you forget to fit a model? See `?workflows::fit`.")
    }
    tree = workflows::extract_fit_engine(tree)
    y_name = names(fitted$pre$mold$outcomes)[[1]]
    attr(tree$terms, "variables")[[2]] = y_name
    names(attr(tree$terms, "dataClasses"))[[1]] = y_name
    parttree.rpart(tree, keep_as_dt = keep_as_dt, flipaxes = flipaxes)
  }

#' @export
parttree.LearnerClassifRpart = parttree.LearnerRegrRpart =
  function(tree, keep_as_dt = FALSE, flipaxes = FALSE) {
    ## mlr3 front-end
    if (is.null(tree$model)) {
      stop("No model detected.\n",
    	   "Did you forget to assign a learner? See `?mlr3::lrn`.")
    }
    tree = tree$model
    parttree.rpart(tree, keep_as_dt = keep_as_dt, flipaxes = flipaxes)
  }

#' @export
parttree.constparty =
  function(tree, keep_as_dt = FALSE, flipaxes = FALSE) {
    ## sanity checks for tree
    mt = tree$terms
    mf = attr(mt, "factors")
    my = setdiff(rownames(mf), colnames(mf))
    mx = colnames(mf)
    mf = tree$data[0L, , drop = FALSE]
    if((length(my) != 1L) || !(class(mf[[my]])[1L] %in% c("numeric", "integer", "factor", "ordered"))) {
      stop("tree must have a univariate numeric or factor response")
    }
    if((length(mx) > 2L) || any(sapply(mx, function(x) !is.numeric(mf[[x]])))) {
      stop("tree may only have up to two numeric regressors")
    }
    stopifnot(requireNamespace("partykit"))

    ## list of splits kids per node (NULL if terminal)
    splits = as.list(tree$node)
    kids = lapply(splits, "[[", "kids")
    for(i in seq_along(splits)) {
      splits[[i]] = if("split" %in% names(splits[[i]])) splits[[i]]$split else list()
      splits[[i]]$kids = kids[[i]]
    }

    if(length(kids) > 1L) {

      ## recursively add node IDs of children to path
      add_ids = function(x) {
        ki = kids[[x[1L]]]
        if(is.null(ki)) {
          return(list(x))
        } else {
          x = lapply(ki, "c", x)
          return(do.call("c", lapply(x, add_ids)))
        }
      }
      paths = add_ids(1L)

      ## augment splits with interval and label information
      augment_split = function(s) {
        if(length(s) > 0L) {
          ## labels
	  n = length(s$kids)
          if(is.null(s$index)) s$index = 1L:n
          lab = partykit::character_split(s, data = mf)
          lab = paste(lab$name, lab$levels[s$index])
          names(lab) = s$kids
          s$labels = lab

          ## intervals
          int = matrix(rep.int(rep(c(-Inf, Inf), each = n), 2L),
	    nrow = n, ncol = 4L,
            dimnames = list(s$kids, c("xmin", "xmax", "ymin", "ymax")))
          brk = c(-Inf, s$breaks, Inf)
          int[, -3L + 2L * s$varid] = brk[s$index]
          int[, -2L + 2L * s$varid] = brk[s$index + 1L]
          s$intervals = int
        }
        return(s)
      }
      splits = lapply(splits, augment_split)

      ## put together path labels
      labs = character(length(paths))
      for(i in seq_along(paths)) {
        ids = rev(paths[[i]])
        lbs = sapply(1L:(length(ids) - 1L), function(j) {
          splits[[ids[j]]]$labels[as.character(ids[j + 1L])]
        })
        labs[i] = paste(lbs, collapse = " --> ")
      }

      ## combine intervals
      ints = matrix(NA, nrow = length(paths), ncol = 4L)
      for(i in seq_along(paths)) {
        ids = rev(paths[[i]])
        its = sapply(1L:(length(ids) - 1L), function(j) {
          splits[[ids[j]]]$intervals[as.character(ids[j + 1L]), ]
        })
        ints[i, c(1L, 3L)] = apply(its[c(1L, 3L), , drop = FALSE], 1L, max, na.rm = TRUE)
        ints[i, c(2L, 4L)] = apply(its[c(2L, 4L), , drop = FALSE], 1L, min, na.rm = TRUE)
      }
    } else {
      paths = list(1L)
      labs = ""
      ints = matrix(c(-Inf, Inf, -Inf, Inf), nrow = 1L, ncol = 4L)
    }

    ## obtain fitted response
    fit = tree$fitted
    fit = if(is.numeric(fit[["(response)"]])) {
      tapply(fit[["(response)"]], fit[["(fitted)"]], mean, na.rm = TRUE)
    } else {
      fit0 = tapply(fit[["(response)"]], fit[["(fitted)"]], function(y) {
        tab = table(y)
	names(tab)[which.max(tab)]
      })
      factor(fit0, levels = levels(fit[["(response)"]]), ordered = is.ordered(fit[["(response)"]]))
    }

    ## collect everything
    rval = data.frame(
      node = sapply(paths, "[", 1L),
      response = fit,
      path = labs
    )
    names(rval)[2L] = my
    rval = cbind(rval, if(flipaxes) ints[, c(3L:4L, 1L:2L, drop = FALSE)] else ints)
    colnames(rval)[4L:7L] = c("xmin", "xmax", "ymin", "ymax")

    ## turn into data.table?
    if(keep_as_dt) rval = data.table::as.data.table(rval)

    return(rval)
}
