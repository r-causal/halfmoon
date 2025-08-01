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

abort <- function(.message, .envir = parent.frame()) {
  cli::cli_abort(message = .message, .envir = .envir)
}

warn <- function(.message, .envir = parent.frame()) {
  cli::cli_warn(message = .message, .envir = .envir)
}

# Function to extract column name from quosure
# Handles both quoted and unquoted column names
get_column_name <- function(quo, arg_name) {
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
          abort("{.code {arg_name}} must be a column name (quoted or unquoted)")
        }
      )
      
      # Handle different types of evaluated values
      if (is.character(val) && length(val) == 1) {
        val
      } else if (is.symbol(val)) {
        as.character(val)
      } else {
        abort("{.code {arg_name}} must be a column name (quoted or unquoted)")
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
  ".weights"
))

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
