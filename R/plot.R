#' @title Plot decision tree partitions
#' @description Provides a plot method for parttree objects.
#' @returns No return value, called for side effect of producing a plot.
#' @param object A [parttree] data frame.
#' @param raw Logical. Should the raw (i.e., original) data be plotted alongside
#'    the tree partitions? Default is `TRUE`.
#' @param border Colour of the partition borders (edges). Default is "black". To
#'   remove the borders altogether, specify as `NA`.
#' @param fill_alpha Numeric in the range `[0,1]`. Alpha transparency of the
#'   filled partition rectangles. Default is `0.3`.
#' @param ... Additional arguments passed down to
#'   \code{\link[graphics]{tinyplot}}[tinyplot].
#' @param raw Logical. Should the raw (original) data points be plotted too?
#'   Default is TRUE.
#' @importFrom tinyplot tinyplot
#' @export
#' @examples
#' ## rpart tree example
#' library("rpart")
#' rp = rpart(Kyphosis ~ Start + Age, data = kyphosis)
#' pt = parttree(rp)
#'
#' ## simple plot
#' plot(pt)
#'
#' ## removing the (recursive) partition borders helps to emphasise overall fit
#' plot(pt, border = NA)
#'
#' ## customize further by passing extra options to (tiny)plot
#' plot(
#'     pt,
#'     border  = NA,                                     # no partition borders
#'     pch     = 19,                                     # filled points
#'     alpha   = 0.6,                                    # point transparency
#'     grid    = TRUE,                                   # background grid
#'     palette = "classic",                              # new colour palette
#'     xlab    = "Topmost vertebra operated on",         # custom x title
#'     ylab    = "Patient age (months)",                 # custom y title
#'     main    = "Tree predictions: Kyphosis recurrence" # custom title
#' )
plot.parttree =
    function(
        object,
        raw = TRUE,
        border = "black",
        fill_alpha = 0.3,
        xlab = NULL,
        ylab = NULL,
        ...) {

        xvar = attr(object, "parttree")[["xvar"]]
        yvar = attr(object, "parttree")[["yvar"]]
        xrange = attr(object, "parttree")[["xrange"]]
        yrange = attr(object, "parttree")[["yrange"]]
        response = attr(object, "parttree")[["response"]]
        raw_data = attr(object, "parttree")[["raw_data"]]
        orig_call = attr(object, "parttree")[["call"]]
        orig_na_idx = attr(object, "parttree")[["na.action"]]

        if (is.null(xlab)) xlab = xvar
        if (is.null(ylab)) ylab = yvar

        if (isTRUE(raw)) {
            if (!is.null(raw_data)) {
                raw_data = eval(raw_data)
            } else {
                raw_data = eval(orig_call$data)[, c(response, xvar, yvar)]
                if (!is.null(orig_na_idx)) raw_data = raw_data[-orig_na_idx, , drop = FALSE]
            }
            if (is.null(raw_data)){
                warning(
                    "\nCould not find original data. Ignoring.",
                    "\n(Did you delete the original model object?)\n"
                )
                raw = FALSE
            }

        }

        ## First adjust our parttree object to better fit some base R graphics
        ## requirements

        xmin_idxr = object$xmin == -Inf
        xmax_idxr = object$xmax == Inf
        ymin_idxr = object$ymin == -Inf
        ymax_idxr = object$ymax == Inf

        object$xmin[xmin_idxr] = xrange[1]
        object$xmax[xmax_idxr] = xrange[2]
        object$ymin[ymin_idxr] = yrange[1]
        object$ymax[ymax_idxr] = yrange[2]

        ## Start plotting...

        plot_fml = reformulate(paste(xvar, "|", response), response = yvar)

        # First draw empty plot (since we need the plot corners to correctly
        # expand the partition limits to the edges of the plot). We'll create a
        # dummy object for this task.
        dobj = data.frame(
            response = rep(object[[response]], 2),
            x = c(object[["xmin"]], object[["xmax"]]),
            y = c(object[["ymin"]], object[["ymax"]])
        )
        colnames(dobj) = c(response, xvar, yvar)

        tinyplot(
                plot_fml,
                data = dobj,
                type = "rect",
                col = border,
                fill = fill_alpha,
                empty = TRUE,
                ...
        )
        object$response = object[[response]]

        # # First draw empty plot (since we need the plot corners to correctly
        # # expand the partition limits to the edges of the plot)
        # with(
        #     object,
        #     tinyplot(
        #         xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax,
        #         by = response,
        #         type = "rect",
        #         col = border,
        #         fill = fill_alpha,
        #         xlab = xlab, ylab = ylab,
        #         legend = list(title = NULL),
        #         empty = TRUE,
        #         ...
        #     )
        # )

        # Grab the plot corners and adjust the partition limits
        corners = par("usr")
        object$xmin[xmin_idxr] = corners[1]
        object$xmax[xmax_idxr] = corners[2]
        object$ymin[ymin_idxr] = corners[3]
        object$ymax[ymax_idxr] = corners[4]

        # Add the (adjusted) partition rectangles
        with(
            object,
            tinyplot(
                xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax,
                by = response,
                type = "rect",
                add = TRUE,
                col = border,
                fill = fill_alpha,
                ...
            )
        )

        # Add the original data points (if requested)
        if (isTRUE(raw)) {
            tinyplot(
                plot_fml,
                data = raw_data,
                type = "p",
                add = TRUE,
                ...
            )
        }

    }

