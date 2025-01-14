#' @title Plot decision tree partitions
#' @description Provides a plot method for parttree objects.
#' @returns No return value, called for side effect of producing a plot.
#' @param x A [parttree] data frame.
#' @param raw Logical. Should the raw (i.e., original) data be plotted alongside
#'    the tree partitions? Default is `TRUE`.
#' @param border Colour of the partition borders (edges). Default is "black". To
#'   remove the borders altogether, specify as `NA`.
#' @param fill_alpha Numeric in the range `[0,1]`. Alpha transparency of the
#'   filled partition rectangles. Default is `0.3`.
#' @param expand Logical. Should the partition limits be expanded to to meet the
#'   edge of the plot axes? Default is `TRUE`. If `FALSE`, then the partition
#'   limits will extend only until the range of the raw data.
#' @param jitter Logical. Should the raw points be jittered? Default is `FALSE`.
#'   Only evaluated if `raw = TRUE`.
#' @param add Logical. Add to an existing plot? Default is `FALSE`.
#' @param ... Additional arguments passed down to
#'   \code{\link[tinyplot]{tinyplot}}.
#' @param raw Logical. Should the raw (original) data points be plotted too?
#'   Default is TRUE.
#' @returns No return value; called for its side effect of producing a plot.
#' @importFrom stats reformulate
#' @importFrom graphics par
#' @importFrom tinyplot tinyplot
#' @rdname plot.parttree
#' @inherit parttree examples
#' @export
plot.parttree = function(
    x,
    raw = TRUE,
    border = "black",
    fill_alpha = 0.3,
    expand = TRUE,
    jitter = FALSE,
    add = FALSE,
    ...
    ) {
    dots = list(...)
    object = x
    flip_data = isTRUE(attr(object, "parttree")[["flip"]])
    if (flip_data) {
      xvar = attr(object, "parttree")[["yvar"]]
      yvar = attr(object, "parttree")[["xvar"]]
      xrange = attr(object, "parttree")[["yrange"]]
      yrange = attr(object, "parttree")[["xrange"]]
    } else {
      xvar = attr(object, "parttree")[["xvar"]]
      yvar = attr(object, "parttree")[["yvar"]]
      xrange = attr(object, "parttree")[["xrange"]]
      yrange = attr(object, "parttree")[["yrange"]]
    }
    response = attr(object, "parttree")[["response"]]
    raw_data = attr(object, "parttree")[["raw_data"]]
    orig_call = attr(object, "parttree")[["call"]]
    orig_na_idx = attr(object, "parttree")[["na.action"]]

    if (isTRUE(raw)) {
        if (!is.null(raw_data)) {
            raw_data = eval(raw_data)
        } else {
            raw_data = eval(orig_call$data)[, c(response, xvar, yvar)]
            if (!is.null(orig_na_idx)) raw_data = raw_data[-orig_na_idx, , drop = FALSE]
        }
        if (is.null(raw_data) || (is.atomic(raw_data) && is.na(raw_data))) {
            warning(
                "\nCould not find original data. Setting `raw = FALSE`.\n"
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

    # First draw an empty plot (since we need the plot corners to correctly
    # expand the partition limits to the edges of the plot). We'll create a
    # dummy object for this task.
    if (isFALSE(add)) {
        dobj = data.frame(
            response = rep(object[[response]], 2),
            x = c(object[["xmin"]], object[["xmax"]]),
            y = c(object[["ymin"]], object[["ymax"]])
        )
        colnames(dobj) = c(response, xvar, yvar)

        if (isTRUE(raw) && isTRUE(jitter)) {
            dobj[[xvar]] = range(c(dobj[[xvar]], jitter(raw_data[[xvar]])), na.rm = TRUE)
            dobj[[yvar]] = range(c(dobj[[yvar]], jitter(raw_data[[yvar]])), na.rm = TRUE)
        }

        tinyplot(
            plot_fml,
            data = dobj,
            type = "rect",
            col = border,
            fill = fill_alpha,
            empty = TRUE,
            ...
        )
    }

    object$response = object[[response]]

    # Grab the plot corners and adjust the partition limits
    if (isTRUE(expand)) {
        corners = par("usr")
        if (isTRUE(dots[["flip"]])) {
          object$xmin[xmin_idxr] = corners[3]
          object$xmax[xmax_idxr] = corners[4]
          object$ymin[ymin_idxr] = corners[1]
          object$ymax[ymax_idxr] = corners[2]
        } else {
          object$xmin[xmin_idxr] = corners[1]
          object$xmax[xmax_idxr] = corners[2]
          object$ymin[ymin_idxr] = corners[3]
          object$ymax[ymax_idxr] = corners[4]
        }
    }


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
        ptype = ifelse(isTRUE(jitter), "j", "p")
        tinyplot(
            plot_fml,
            data = raw_data,
            type = ptype,
            add = TRUE,
            ...
        )
    }
}

