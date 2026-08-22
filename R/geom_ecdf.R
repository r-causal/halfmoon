#' Calculate weighted and unweighted empirical cumulative distributions
#'
#' The empirical cumulative distribution function (ECDF) provides an alternative
#' visualization of distribution. `geom_ecdf()` is similar to
#' [`ggplot2::stat_ecdf()`] but it can also calculate weighted ECDFs.
#'
#' @details
#' ECDF plots show the cumulative distribution function \eqn{F(x) = P(X \leq x)},
#' displaying what proportion of observations fall below each value. When comparing
#' treatment groups, overlapping ECDF curves indicate similar distributions and
#' thus good balance.
#'
#' ECDF plots are closely related to quantile-quantile (QQ) plots (see [`geom_qq2()`]).
#' While ECDF plots show \eqn{F(x)} for each group, QQ plots show the inverse relationship
#' by plotting \eqn{F_1^{-1}(p)} vs \eqn{F_2^{-1}(p)}. Both visualize the same distributional
#' information:
#' - ECDF plots: Compare cumulative probabilities at each value
#' - QQ plots: Compare values at each quantile
#'
#' Choose ECDF plots when you want to see the full cumulative distribution or when
#' comparing multiple groups simultaneously. Choose QQ plots when you want to directly
#' compare two groups with an easy-to-interpret 45-degree reference line.
#'
#' `geom_ecdf()` supports both orientations. Mapping the variable to `y`, or
#' passing `orientation = "y"`, computes the same weighted curve and draws it
#' across the panel instead of up it.
#'
#' @section Aesthetics: In addition to the aesthetics for
#'   [`ggplot2::stat_ecdf()`], `geom_ecdf()` also accepts: \itemize{ \item
#'   weights }
#'
#' @inheritParams ggplot2::stat_ecdf
#' @param orientation The axis the curve runs along, `"x"` or `"y"`. Defaults to
#'   `NA`, which reads the orientation from the aesthetics the layer is given.
#'
#' @return a geom
#' @family ggplot2 functions
#'
#' @seealso
#' - [`geom_qq2()`] for an alternative visualization using quantile-quantile plots
#' - [`ggplot2::stat_ecdf()`] for the unweighted version
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(
#'   nhefs_weights,
#'   aes(x = smokeyrs, color = qsmk)
#' ) +
#'   geom_ecdf(aes(weights = w_ato)) +
#'   xlab("Smoking Years") +
#'   ylab("Proportion <= x")
#'
geom_ecdf <- function(
  mapping = NULL,
  data = NULL,
  geom = "step",
  position = "identity",
  ...,
  n = NULL,
  pad = TRUE,
  na.rm = FALSE,
  orientation = NA,
  show.legend = NA,
  inherit.aes = TRUE
) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatWeightedECDF,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      n = n,
      pad = pad,
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

#' Evaluate a weighted ECDF on the same grid `stat_ecdf()` uses
#'
#' @param x The variable the ECDF is computed over
#' @param weights The weight of each observation, already numeric
#' @param n Number of points to interpolate along, or `NULL` for the observed
#'   values
#' @param pad Add `-Inf` and `Inf` so the curve spans the panel?
#'
#' @return A data frame of `x` and `ecdf`, or `NULL` when the weights carry no
#'   mass and there is no curve to draw.
#' @noRd
compute_weighted_ecdf <- function(x, weights, n = NULL, pad = TRUE) {
  # A negative weight would make the cumulative sum non-monotone, which sends
  # the curve back down and outside [0, 1]. `validate_weights()` refuses one
  # everywhere else, so the geom does too.
  if (any(weights < 0, na.rm = TRUE)) {
    abort(
      "{.field weights} cannot contain negative values",
      error_class = "halfmoon_range_error",
      call = quote(geom_ecdf())
    )
  }

  total <- sum(weights)

  # Every observation would have to contribute nothing, which leaves no
  # distribution to describe rather than a curve of zeroes
  if (total <= 0) {
    warn(
      "Dropping a group whose {.field weights} sum to {.val {total}}",
      warning_class = "halfmoon_data_warning",
      call = quote(geom_ecdf())
    )

    return(NULL)
  }

  ordered <- order(x)
  x <- x[ordered]
  weights <- weights[ordered]

  values <- unique(x)
  cumulative <- cumsum(vapply(
    split(weights, match(x, values)),
    sum,
    numeric(1)
  ))

  grid <- if (is.null(n)) values else seq(min(x), max(x), length.out = n)
  if (pad) {
    grid <- c(-Inf, grid, Inf)
  }

  # A single observed value leaves nothing to interpolate between, so the step
  # is placed by hand
  ecdf <- if (length(values) == 1) {
    ifelse(grid < values, 0, 1)
  } else {
    stats::approxfun(
      values,
      cumulative / total,
      method = "constant",
      yleft = 0,
      yright = 1,
      f = 0,
      ties = "ordered"
    )(grid)
  }

  data.frame(x = grid, ecdf = ecdf)
}

StatWeightedECDF <- ggplot2::ggproto(
  "StatWeightedECDF",
  ggplot2::StatEcdf,
  setup_data = function(data, params) {
    # A weight of `NA` would otherwise spread through the cumulative sum and
    # take the whole curve with it. `remove_missing()` reports the dropped rows
    # unless `na.rm = TRUE` asks for them to go quietly.
    if ("weights" %in% names(data)) {
      data$weights <- extract_weight_data(data$weights)
    }

    ggplot2::remove_missing(
      data,
      na.rm = params$na.rm %||% FALSE,
      vars = intersect(c("x", "weights"), names(data)),
      name = "geom_ecdf"
    )
  },
  compute_group = function(
    data,
    scales,
    n = NULL,
    pad = TRUE,
    flipped_aes = FALSE
  ) {
    # The curve is always computed over `x`; a flipped orientation swaps the
    # aesthetics on the way in and back again on the way out, as the ggplot2
    # stats do
    data <- ggplot2::flip_data(data, flipped_aes)

    result <- if (!"weights" %in% names(data)) {
      ggplot2::StatEcdf$compute_group(data, scales, n = n, pad = pad)
    } else if (nrow(data) == 0) {
      data.frame(x = numeric(0), ecdf = numeric(0))
    } else {
      compute_weighted_ecdf(data$x, data$weights, n = n, pad = pad)
    }

    # A group whose weights carry no mass is dropped rather than drawn
    if (is.null(result)) {
      return(NULL)
    }

    result$y <- result$ecdf
    result$flipped_aes <- flipped_aes
    ggplot2::flip_data(result, flipped_aes)
  },
  required_aes = c("x|y"),
  optional_aes = "weights",
  dropped_aes = c("weight", "weights")
)
