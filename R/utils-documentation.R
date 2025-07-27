# Documentation templates for parameter reuse with roxygen2

#' Parameter Documentation for Balance Functions
#'
#' This function exists solely to document parameters shared across balance functions.
#'
#' @param covariate A numeric vector containing the covariate values to compare.
#' @param group A vector (factor or numeric) indicating group membership. Must
#'   have exactly two unique levels.
#' @param weights An optional numeric vector of case weights. If provided, must
#'   have the same length as other input vectors. All weights must be non-negative.
#' @param reference_group The reference group level for comparisons. Can be either
#'   a group level value or a numeric index. If `NULL` (default), uses the first level.
#' @param na.rm A logical value indicating whether to remove missing values
#'   before computation. If `FALSE` (default), missing values in the input
#'   will produce `NA` in the output.
#' @keywords internal
#' @name balance_params
NULL

#' Parameter Documentation for Check Functions
#'
#' This function exists solely to document parameters shared across check functions.
#'
#' @param .data A data frame containing the variables to analyze.
#' @param .vars Variables for which to calculate metrics. Can be unquoted
#'   variable names, a character vector, or a tidyselect expression.
#' @param .group Grouping variable, e.g., treatment or exposure group.
#' @param .wts Optional weighting variables. Can be unquoted variable names,
#'   a character vector, or NULL. Multiple weights can be provided to compare
#'   different weighting schemes.
#' @param include_observed Logical. If using `.wts`, also calculate observed
#'   (unweighted) metrics? Defaults to TRUE.
#' @keywords internal
#' @name check_params
NULL

#' Parameter Documentation for Treatment Level
#'
#' @param treatment_level The level of the outcome variable to consider as the
#'   treatment/event. If `NULL` (default), uses the last level for factors or
#'   the maximum value for numeric variables.
#' @keywords internal
#' @name treatment_param
NULL

#' Parameter Documentation for ggplot2 Functions
#'
#' @param mapping Set of aesthetic mappings created by `aes()`. If specified and
#'   `inherit.aes = TRUE` (the default), it is combined with the default mapping
#'   at the top level of the plot.
#' @param data The data to be displayed in this layer. If `NULL`, the default,
#'   the data is inherited from the plot data as specified in the call to `ggplot()`.
#' @param stat The statistical transformation to use on the data for this layer.
#' @param position Position adjustment.
#' @param na.rm If `FALSE`, the default, missing values are removed with a warning.
#'   If `TRUE`, missing values are silently removed.
#' @param show.legend Logical. Should this layer be included in the legends?
#'   `NA`, the default, includes if any aesthetics are mapped.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics, rather than
#'   combining with them.
#' @param ... Other arguments passed on to layer().
#' @keywords internal
#' @name ggplot2_params
NULL
