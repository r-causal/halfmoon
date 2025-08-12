#' Autoplot Methods for halfmoon Objects
#'
#' These methods provide automatic plot generation for halfmoon data objects
#' using ggplot2's autoplot interface. Each method dispatches to the appropriate
#' plot_*() function as follows:
#'
#' - `autoplot.halfmoon_balance` calls [plot_balance()]
#' - `autoplot.halfmoon_ess` calls [plot_ess()]
#' - `autoplot.halfmoon_calibration` calls [plot_model_calibration()]
#' - `autoplot.halfmoon_roc` calls [plot_model_roc_curve()]
#' - `autoplot.halfmoon_auc` calls [plot_model_auc()]
#' - `autoplot.halfmoon_qq` calls [plot_qq()]
#'
#' @param object A halfmoon data object with appropriate class
#' @param ... Additional arguments passed to the underlying plot_*() function
#'
#' @return A ggplot2 object
#' @name autoplot-halfmoon
NULL

#' @rdname autoplot-halfmoon
#' @export
#' @importFrom ggplot2 autoplot
autoplot.halfmoon_balance <- function(object, ...) {
  plot_balance(object, ...)
}

#' @rdname autoplot-halfmoon
#' @export
autoplot.halfmoon_ess <- function(object, ...) {
  plot_ess(object, ...)
}

#' @rdname autoplot-halfmoon
#' @export
autoplot.halfmoon_calibration <- function(object, ...) {
  plot_model_calibration(object, ...)
}

#' @rdname autoplot-halfmoon
#' @export
autoplot.halfmoon_roc <- function(object, ...) {
  plot_model_roc_curve(object, ...)
}

#' @rdname autoplot-halfmoon
#' @export
autoplot.halfmoon_auc <- function(object, ...) {
  plot_model_auc(object, ...)
}

#' @rdname autoplot-halfmoon
#' @export
autoplot.halfmoon_qq <- function(object, ...) {
  plot_qq(object, ...)
}

#' Plot Methods for halfmoon Objects
#'
#' These methods provide standard plot generation for halfmoon data objects.
#' They create the plot using autoplot() and then print it.
#'
#' @param x A halfmoon data object with appropriate class
#' @param ... Additional arguments passed to autoplot()
#'
#' @return Invisibly returns the ggplot2 object after printing
#' @name plot-halfmoon
NULL

#' @rdname plot-halfmoon
#' @export
plot.halfmoon_balance <- function(x, ...) {
  autoplot(x, ...)
}

#' @rdname plot-halfmoon
#' @export
plot.halfmoon_ess <- function(x, ...) {
  autoplot(x, ...)
}

#' @rdname plot-halfmoon
#' @export
plot.halfmoon_calibration <- function(x, ...) {
  autoplot(x, ...)
}

#' @rdname plot-halfmoon
#' @export
plot.halfmoon_roc <- function(x, ...) {
  autoplot(x, ...)
}

#' @rdname plot-halfmoon
#' @export
plot.halfmoon_auc <- function(x, ...) {
  autoplot(x, ...)
}

#' @rdname plot-halfmoon
#' @export
plot.halfmoon_qq <- function(x, ...) {
  autoplot(x, ...)
}
