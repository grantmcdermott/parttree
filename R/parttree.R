#' @title Convert a decision tree into a data frame of partition coordinates
#' @aliases parttree parttree.rpart parttree._rpart parttree.workflow
#'   parttree.LearnerClassifRpart parttree.LearnerRegrRpart parttree.constparty
#' @description Extracts the terminal leaf nodes of a decision tree that
#'   contains no more that two numeric predictor variables. These leaf nodes are
#'   then converted into a data frame, where each row represents a partition (or
#'   leaf or terminal node) that can easily be plotted in 2-D coordinate space.
#' @param tree An \code{\link[rpart]{rpart.object}} or alike. This includes
#'   compatible classes from the `mlr3` and `tidymodels` frontends, or the
#'   `constparty` class inheriting from \code{\link[partykit]{party}}.
#' @param keep_as_dt Logical. The function relies on `data.table` for internal
#'   data manipulation. But it will coerce the final return object into a
#'   regular data frame (default behavior) unless the user specifies `TRUE`.
#' @param flip Logical. Should we flip the "x" and "y" variables in the return
#'   data frame? The default behaviour is for the first split variable in the
#'   tree to take the "y" slot, and any second split variable to take the "x"
#'   slot. Setting to `TRUE` switches these around.
#'
#'   _Note:_ This argument is primarily useful when it passed via
#'   [geom_parttree] to ensure correct axes orientation as part of a `ggplot2`
#'   visualization (see [geom_parttree] Examples). We do not expect users to
#'   call `parttree(..., flip = TRUE)` directly. Similarly, to switch axes
#'   orientation for the native (base graphics) [plot.parttree] method, we
#'   recommend calling `plot(..., flip = TRUE)` rather than flipping the
#'   underlying `parttree` object.
#' @seealso [plot.parttree], [geom_parttree], \code{\link[rpart]{rpart}},
#'   \code{\link[partykit]{ctree}} [partykit::ctree].
#' @returns A data frame comprising seven columns: the leaf node, its path, a
#'   set of rectangle limits (i.e., xmin, xmax, ymin, ymax), and a final column
#'   corresponding to the predicted value for that leaf.
#' @importFrom data.table := .SD as.data.table data.table fifelse rbindlist tstrsplit
#' @importFrom rpart path.rpart
#' @importFrom stats reformulate terms
#' @export
#' @examples
#' library("parttree")
#' \dontshow{data.table::setDTthreads(2)}
#' #
#' ## rpart trees
#'
#' library("rpart")
#' rp = rpart(Kyphosis ~ Start + Age, data = kyphosis)
#'
#' # A parttree object is just a data frame with additional attributes
#' (rp_pt = parttree(rp))
#' attr(rp_pt, "parttree")
#'
#' # simple plot
#' plot(rp_pt)
#'
#' # removing the (recursive) partition borders helps to emphasise overall fit
#' plot(rp_pt, border = NA)
#'
#' # customize further by passing extra options to (tiny)plot
#' plot(
#'    rp_pt,
#'    border  = NA,                                     # no partition borders
#'    pch     = 16,                                     # filled points
#'    alpha   = 0.6,                                    # point transparency
#'    grid    = TRUE,                                   # background grid
#'    palette = "classic",                              # new colour palette
#'    xlab    = "Topmost vertebra operated on",         # custom x title
#'    ylab    = "Patient age (months)",                 # custom y title
#'    main    = "Tree predictions: Kyphosis recurrence" # custom title
#' )
#'
#' #
#' ## conditional inference trees from partyit
#'
#' library("partykit")
#' ct = ctree(Species ~ Petal.Length + Petal.Width, data = iris)
#' ct_pt = parttree(ct)
#' plot(ct_pt, pch = 19, palette = "okabe", main = "ctree predictions: iris species")
#'
#' ## rpart via partykit
#' rp2 = as.party(rp)
#' parttree(rp2)
#'
#' #
#' ## various front-end frameworks are also supported, e.g.
#'
#' # tidymodels
#'
#' # install.packages("parsnip")
#' library(parsnip)
#'
#' decision_tree() |>
#'   set_engine("rpart") |>
#'   set_mode("classification") |>
#'   fit(Species ~ Petal.Length + Petal.Width, data=iris) |>
#'   parttree() |>
#'   plot(main = "This time brought to you via parsnip...")
#'
#' # mlr3 (NB: use `keep_model = TRUE` for mlr3 learners)
#'
#' # install.packages("mlr3")
#' library(mlr3)
#'
#' task_iris = TaskClassif$new("iris", iris, target = "Species")
#' task_iris$formula(rhs = "Petal.Length + Petal.Width")
#' fit_iris = lrn("classif.rpart", keep_model = TRUE) # NB!
#' fit_iris$train(task_iris)
#' plot(parttree(fit_iris), main = "... and now mlr3")
#'
parttree =
  function(tree, keep_as_dt = FALSE, flip = FALSE) {
    UseMethod("parttree")
  }

#' @export
parttree.rpart =
  function(tree, keep_as_dt = FALSE, flip = FALSE, ...) {
    ## Silence NSE notes in R CMD check. See:
    ## https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html#globals
    V1 = node = path = variable = side = ..vars = xvar = yvar = xmin = xmax = ymin = ymax = NULL

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
    all_terms = terms(tree)
    y_var = all.vars(all_terms)[attr(all_terms, "response")]
    ### y values
    yvals = tree$frame[tree$frame$var == "<leaf>", ]$yval
    # y_factored = attr(tree$terms, "dataClasses")[paste(y_var)] == "factor"
    y_factored = attr(tree$terms, "dataClasses")[y_var] == "factor"
    ## factor equivalents (if factor)
    if (y_factored) {
      ylevs = attr(tree, "ylevels")
      yvals = ylevs[yvals]
    }

    part_list = path.rpart(tree, nodes = nodes, print.it = FALSE)
    part_list = lapply(part_list, as.data.table)
    part_dt = rbindlist(part_list, idcol="node")[V1!="root"]
    part_dt[, c("variable", "split") := tstrsplit(V1, split = "+<|+<=|>|+>=")][]
    part_dt[, side := gsub("\\s$", "", gsub("\\w|\\.", "", V1))][]

    yvals_dt = data.table(yvals, node = nodes)

    part_dt = part_dt[yvals_dt, on = "node", all = TRUE]
    part_dt[, V1 := NULL][, node := as.integer(node)][, split := as.double(split)][]

    ## Trim irrelevant parts of tree
    data.table::setorder(part_dt, node)

    part_dt[
      ,
      path := paste(variable, side, split, collapse = " --> "),
      by = node
    ]

    part_dt = part_dt[
      ,
      .SD[(grepl(">", side) & split == max(split)) | (grepl("<", side) & split == min(split))],
      keyby = list(node, variable, side)
    ]

    ## Get the coords data frame

    if (length(vars)==2) {
      ## special case we can assume is likely wrong, notwithstanding ability to still flip axes
      if (vars[1]=="y" & vars[2]=="x") vars = rev(vars)
    } else if (length(vars)==1) {
      ## Handle edge cases with only 1 level
      missing_var = setdiff(attr(tree$terms, 'term.labels'), vars)
      vars = c(missing_var, vars)
    }
    if (flip) vars = rev(vars)

    part_coords =
      part_dt[
        ,
        `:=` (xvar = variable == ..vars[1],
              yvar = variable == ..vars[2])
      ][
        ,
        `:=` (xmin = fifelse(xvar & grepl(">", side), split, NA_real_),
              xmax = fifelse(xvar & grepl("<", side), split, NA_real_),
              ymin = fifelse(yvar & grepl(">", side), split, NA_real_),
              ymax = fifelse(yvar & grepl("<", side), split, NA_real_))
      ][
        ,
        list(xmin = mean(xmin, na.rm = TRUE),
             xmax = mean(xmax, na.rm = TRUE),
             ymin = mean(ymin, na.rm = TRUE),
             ymax = mean(ymax, na.rm = TRUE)),
        keyby = list(node, yvals, path)
      ][
        ,
        `:=` (xmin = fifelse(is.na(xmin), -Inf, xmin),
              xmax = fifelse(is.na(xmax),  Inf, xmax),
              ymin = fifelse(is.na(ymin), -Inf, ymin),
              ymax = fifelse(is.na(ymax),  Inf, ymax))
      ]

    if (y_factored) {
      part_coords$yvals = factor(part_coords$yvals, levels = ylevs)
    }
    colnames(part_coords) = gsub("yvals", y_var, colnames(part_coords))

    if (!keep_as_dt) {
      part_coords = as.data.frame(part_coords)
    }

    class(part_coords) = c("parttree", class(part_coords))

    # attributes (for plot method)
    dots = list(...)
    if (!is.null(dots[["xvar"]])) {
      xvar = dots[["xvar"]]
    } else {
      xvar = ifelse(isFALSE(flip), vars[1], vars[2])
    }
    if (!is.null(dots[["yvar"]])) {
      yvar = dots[["yvar"]]
    } else {
      yvar = ifelse(isFALSE(flip), vars[2], vars[1])
    }
    if (!is.null(dots[["xrange"]])) {
      xrange = dots[["xrange"]]
    } else {
      xrange = range(eval(tree$call$data, envir = attr(tree$terms, ".Environment"))[[xvar]], na.rm = TRUE)
    }
    if (!is.null(dots[["yrange"]])) {
      yrange = dots[["yrange"]]
    } else {
      yrange = range(eval(tree$call$data, envir = attr(tree$terms, ".Environment"))[[yvar]], na.rm = TRUE)
    }
    raw_data = orig_call = orig_na.action = NULL
    if (!is.null(dots[["raw_data"]])) {
      raw_data = substitute(dots[["raw_data"]])
    } else {
      orig_call = tree$call
      orig_na.action = tree$na.action
    }

    attr(part_coords, "parttree") = list(
      xvar = xvar,
      yvar = yvar,
      xrange = xrange,
      yrange = yrange,
      response = y_var,
      call = orig_call,
      na.action = orig_na.action,
      flip = flip,
      raw_data = raw_data
      )

    return(part_coords)
  }

#' @export
parttree._rpart =
  function(tree, keep_as_dt = FALSE, flip = FALSE) {
    ## parsnip front-end
    if (is.null(tree$fit)) {
      stop("No model detected.\n",
    	   "Did you forget to fit a model? See `?parsnip::fit`.")
    }
    tree = tree$fit
    # extra attribute arguments to pass through ... to parttree.rpart
    raw_data = attr(tree$terms, ".Environment")$data
    vars = attr(tree$terms, "term.labels")
    xvar = ifelse(isFALSE(flip), vars[1], vars[2])
    yvar = ifelse(isFALSE(flip), vars[2], vars[1])
    xrange = range(raw_data[[xvar]])
    yrange = range(raw_data[[yvar]])

    parttree.rpart(
      tree, keep_as_dt = keep_as_dt, flip = flip,
      raw_data = raw_data,
      xvar = xvar, yvar = yvar,
      xrange = xrange, yrange = yrange
    )
  }

#' @export
parttree.workflow =
  function(tree, keep_as_dt = FALSE, flip = FALSE) {
    ## workflow front-end
    if (!workflows::is_trained_workflow(tree)) {
      stop("No model detected.\n",
           "Did you forget to fit a model? See `?workflows::fit`.")
    }
    y_name = names(tree$pre$mold$outcomes)[[1]]
    raw_data = cbind(tree$pre$mold$predictors, tree$pre$mold$outcomes)
    tree = workflows::extract_fit_engine(tree)
    tree$terms[[2]] = reformulate(y_name)[[2]]
    attr(tree$terms, "variables")[[2]] = y_name
    names(attr(tree$terms, "dataClasses"))[[1]] = y_name

    # extra attribute arguments to pass through ... to parttree.rpart
    vars = attr(tree$terms, "term.labels")
    xvar = ifelse(isFALSE(flip), vars[1], vars[2])
    yvar = ifelse(isFALSE(flip), vars[2], vars[1])
    xrange = range(raw_data[[xvar]])
    yrange = range(raw_data[[yvar]])

    parttree.rpart(
      tree, keep_as_dt = keep_as_dt, flip = flip,
      raw_data = raw_data,
      xvar = xvar, yvar = yvar,
      xrange = xrange, yrange = yrange
    )
  }

#' @export
parttree.LearnerClassifRpart =
  function(tree, keep_as_dt = FALSE, flip = FALSE) {
    ## mlr3 front-end
    if (is.null(tree$model)) {
      stop("No model detected.\n",
    	   "Did you forget to assign a learner? See `?mlr3::lrn`.")
    }

    pars = tree$param_set$get_values()
    keep_model = isTRUE(pars$keep_model)

    tree = tree$model

    # extra attribute arguments to pass through ... to parttree.rpart
    # raw_data = eval(tree$call$data)
    vars = attr(tree$terms, "term.labels")
    xvar = ifelse(isFALSE(flip), vars[1], vars[2])
    yvar = ifelse(isFALSE(flip), vars[2], vars[1])
    if (keep_model) {
      raw_data = tree$model
      xrange = range(raw_data[[xvar]])
      yrange = range(raw_data[[yvar]])
    } else {
      raw_data = NA
      xrange = NA
      yrange = NA
      message(
        "\nUnable to retrieve the original data, which we need for the default plot.parttree method.",
        "\nFor mlr3 workflows, we recommended an explicit call to `keep_model = TRUE` when defining your Learner before training the model.\n"
      )
    }

    parttree.rpart(
      tree, keep_as_dt = keep_as_dt, flip = flip,
      raw_data = raw_data,
      xvar = xvar, yvar = yvar,
      xrange = xrange, yrange = yrange
    )
  }

#' @export
parttree.LearnerRegrRpart = parttree.LearnerClassifRpart

#' @export
parttree.constparty =
  function(tree, keep_as_dt = FALSE, flip = FALSE) {
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
    rval = cbind(rval, if(flip) ints[, c(3L:4L, 1L:2L, drop = FALSE)] else ints)
    colnames(rval)[4L:7L] = c("xmin", "xmax", "ymin", "ymax")

    ## turn into data.table?
    if(keep_as_dt) rval = as.data.table(rval)

    class(rval) = c("parttree", class(rval))
    xvar = ifelse(isFALSE(flip), mx[1], mx[2])
    yvar = ifelse(isFALSE(flip), mx[2], mx[1])
    attr(rval, "parttree") = list(
      xvar = xvar,
      yvar = yvar,
      xrange = range(eval(tree$data)[[xvar]], na.rm = TRUE),
      yrange = range(eval(tree$data)[[yvar]], na.rm = TRUE),
      response = my,
      call = NULL,
      na.action = NULL,
      flip = flip,
      raw_data = substitute(tree$data) # Or, partykit::model_frame_rpart?
      )

    return(rval)
}
