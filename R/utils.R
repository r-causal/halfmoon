# future implementations for interface
#
# be_quiet <- function() {
#   getOption("halfmoon.quiet", default = FALSE)
# }
#
# alert_info <- function(.message) {
#   if (!be_quiet()) {
#     cli::cli_alert_info(text = .message)
#   }
# }

# Import NULL coalescing operator from rlang
#' @importFrom rlang %||%
NULL

abort <- function(
  ...,
  error_class = NULL,
  call = rlang::caller_env(),
  .envir = parent.frame()
) {
  cli::cli_abort(
    ...,
    class = c(error_class, "halfmoon_error"),
    call = call,
    .envir = .envir
  )
}

warn <- function(
  ...,
  warning_class = NULL,
  call = rlang::caller_env(),
  .envir = parent.frame()
) {
  cli::cli_warn(
    ...,
    class = c(warning_class, "halfmoon_warning"),
    call = call,
    .envir = .envir
  )
}

# Function to extract column name from quosure
# Handles both quoted and unquoted column names
get_column_name <- function(quo, arg_name, call = rlang::caller_env()) {
  # Check if the quosure represents a missing argument
  if (rlang::quo_is_missing(quo)) {
    abort(
      "Argument {.arg {arg_name}} is required",
      error_class = "halfmoon_arg_error",
      call = call
    )
  }
  
  # First try as_name (works for symbols and strings)
  tryCatch(
    {
      rlang::as_name(quo)
    },
    error = function(e) {
      # If as_name fails, try to evaluate the quosure
      val <- tryCatch(
        {
          rlang::eval_tidy(quo)
        },
        error = function(e2) {
          abort(
          "{.code {arg_name}} must be a column name (quoted or unquoted)",
          error_class = "halfmoon_type_error"
        )
        }
      )

      # Handle different types of evaluated values
      if (is.character(val) && length(val) == 1) {
        val
      } else if (is.symbol(val)) {
        as.character(val)
      } else {
        abort(
          "{.code {arg_name}} must be a column name (quoted or unquoted)",
          error_class = "halfmoon_type_error"
        )
      }
    }
  )
}

utils::globalVariables(c(
  "method",
  "metric",
  "variable",
  "estimate",
  ".bin",
  "bin",
  "n_events",
  "n_total",
  "x",
  "x_var",
  "y",
  "y_var",
  "fitted_mean",
  "group_mean",
  ".fitted",
  ".group",
  "ymin",
  "ymax",
  "x_quantiles",
  "y_quantiles",
  "weight",
  ".var",
  ".weights",
  "group_level"
))

# Error Classes for halfmoon
# These are used with abort() to provide consistent error handling
#
# Type errors
# - halfmoon_type_error: Wrong input type (e.g., non-numeric when numeric expected)
#
# Column/variable errors  
# - halfmoon_column_error: Column not found in data frame
#
# Length/size errors
# - halfmoon_length_error: Length mismatch between vectors or wrong vector length
#
# Empty data errors
# - halfmoon_empty_error: Empty data frame, vector, or no variables selected
#
# Value/range errors
# - halfmoon_range_error: Values outside valid range (e.g., negative weights, invalid indices)
#
# Group/level errors
# - halfmoon_group_error: Wrong number of groups/levels (e.g., need exactly 2 for binary)
#
# Reference errors
# - halfmoon_reference_error: Reference group/value not found in data
#
# Missing value errors
# - halfmoon_na_error: Missing values present when not allowed
#
# Argument errors
# - halfmoon_arg_error: Invalid argument value or missing required argument
#
# Formula/model errors
# - halfmoon_formula_error: Invalid formula or model specification
#
# Aesthetic errors
# - halfmoon_aes_error: Missing required aesthetic mapping (for ggplot2 geoms)
#
# Selection errors
# - halfmoon_select_error: tidyselect returned wrong number of variables

# Warning Classes for halfmoon
# These are used with warn() to provide consistent warning handling
#
# Convergence warnings
# - halfmoon_convergence_warning: Model did not converge
#
# Data warnings
# - halfmoon_data_warning: Issues with data (constant values, no events, etc.)
#
# Method warnings
# - halfmoon_method_warning: Invalid or unsupported method specified

#' Create dummy variables from categorical data
#'
#' Internal function to convert categorical variables (factors and characters)
#' into dummy variables. Binary variables (2 levels) are converted to single
#' 0/1 numeric variables, while multi-level variables (3+ levels) are expanded
#' into separate dummy variables for each level.
#'
#' @param data A data frame containing the variables to convert
#' @param binary_as_single Logical. If TRUE (default), binary variables are
#'   kept as single 0/1 variables. If FALSE, they are expanded to dummy
#'   variables for each level.
#'
#' @return A data frame with categorical variables replaced by their dummy
#'   variable representations
#'
#' @noRd
create_dummy_variables <- function(data, binary_as_single = TRUE) {
  # Identify categorical variables (factors and character variables)
  categorical_vars <- purrr::map_lgl(
    data,
    \(x) is.factor(x) || is.character(x)
  )

  if (!any(categorical_vars)) {
    # No categorical variables, return data as-is
    return(data)
  }

  # Extract categorical and non-categorical data
  categorical_data <- dplyr::select(
    data,
    dplyr::where(\(x) is.factor(x) || is.character(x))
  )

  non_categorical_data <- dplyr::select(
    data,
    -dplyr::where(\(x) is.factor(x) || is.character(x))
  )

  # Create dummy variables using functional programming
  dummy_vars <- purrr::imap(
    categorical_data,
    \(col_data, col_name) {
      # Get levels based on variable type
      levels_to_use <- if (is.factor(col_data)) {
        levels(col_data)
      } else {
        sort(unique(col_data))
      }

      # For binary variables, decide based on binary_as_single parameter
      if (length(levels_to_use) == 2 && binary_as_single) {
        # Binary variable: keep as single variable, convert to 0/1 numeric
        # Use the factor levels directly, converting to numeric 0/1
        if (is.factor(col_data)) {
          dummy_values <- as.numeric(col_data) - 1 # Convert to 0/1 from 1/2
        } else {
          # For character, create factor first to ensure consistent ordering
          col_factor <- factor(col_data, levels = levels_to_use)
          dummy_values <- as.numeric(col_factor) - 1
        }
        stats::setNames(list(dummy_values), col_name)
      } else {
        # Multi-level variable or binary with binary_as_single = FALSE:
        # create dummy for each level
        dummy_names <- paste0(col_name, levels_to_use)
        dummy_values <- purrr::map(
          levels_to_use,
          \(x) as.numeric(col_data == x)
        )
        stats::setNames(dummy_values, dummy_names)
      }
    }
  )

  # Flatten and convert to tibble
  dummy_df <- dplyr::as_tibble(purrr::flatten(dummy_vars))

  # Combine non-categorical and dummy variables
  dplyr::bind_cols(non_categorical_data, dummy_df)
}

# Create a signature for a group based on aesthetic columns
create_group_signature <- function(group_data, aes_cols) {
  if (length(aes_cols) > 0) {
    paste(group_data[1, aes_cols, drop = FALSE], collapse = "_")
  } else {
    "no_aes"
  }
}
