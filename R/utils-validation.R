# Validation helper functions for the halfmoon package

# Numeric validation
validate_numeric <- function(
  x,
  arg_name = deparse(substitute(x)),
  call = rlang::caller_env()
) {
  if (!is.numeric(x)) {
    abort(
      "{.arg {arg_name}} must be numeric, got {.cls {class(x)[1]}}",
      error_class = "halfmoon_type_error",
      call = call
    )
  }
  invisible(x)
}

# Single TRUE/FALSE validation
validate_flag <- function(
  x,
  arg_name = deparse(substitute(x)),
  call = rlang::caller_env()
) {
  if (!is.logical(x) || length(x) != 1 || is.na(x)) {
    abort(
      "{.arg {arg_name}} must be a single {.code TRUE} or {.code FALSE}",
      error_class = "halfmoon_arg_error",
      call = call
    )
  }
  invisible(x)
}

# Weight validation. `n` is the length the weights must match; it defaults to
# their own length, which skips that check for a caller that has nothing to
# match them against. `allow_null` is for the callers where weights are
# optional, `NULL` meaning unweighted.
validate_weights <- function(
  weights,
  n = length(weights),
  arg_name = ".weights",
  allow_null = TRUE,
  call = rlang::caller_env()
) {
  if (is.null(weights)) {
    if (allow_null) {
      return(invisible(weights))
    }

    abort(
      "{.arg {arg_name}} must be numeric or a causal weight object, not {.code NULL}",
      error_class = "halfmoon_type_error",
      call = call
    )
  }

  # Accept numeric vectors and any causal weight object, which covers the psw
  # objects from propensity and the bw objects from balancing
  is_valid_weights <- is.numeric(weights) ||
    causalgenerics::is_causal_wt(weights)

  if (!is_valid_weights) {
    type_message <- if (allow_null) {
      "{.arg {arg_name}} must be numeric, a causal weight object, or {.code NULL}"
    } else {
      "{.arg {arg_name}} must be numeric or a causal weight object"
    }
    abort(
      type_message,
      error_class = "halfmoon_type_error",
      call = call
    )
  }
  if (length(weights) != n) {
    abort(
      "{.arg {arg_name}} must have length {n}, got {length(weights)}",
      error_class = "halfmoon_length_error",
      call = call
    )
  }
  if (any(vctrs::vec_data(weights) < 0, na.rm = TRUE)) {
    abort(
      "{.arg {arg_name}} cannot contain negative values",
      error_class = "halfmoon_range_error",
      call = call
    )
  }
  invisible(weights)
}

# Length validation
validate_equal_length <- function(
  x,
  y,
  x_name = NULL,
  y_name = NULL,
  call = rlang::caller_env()
) {
  x_name <- x_name %||% deparse(substitute(x))
  y_name <- y_name %||% deparse(substitute(y))

  x_len <- if (is.matrix(x) || is.data.frame(x)) nrow(x) else length(x)
  y_len <- if (is.matrix(y) || is.data.frame(y)) nrow(y) else length(y)

  if (x_len != y_len) {
    abort(
      "{.arg {x_name}} and {.arg {y_name}} must have the same length",
      error_class = "halfmoon_length_error",
      call = call
    )
  }
  invisible(TRUE)
}

# Non-empty validation
validate_not_empty <- function(
  x,
  arg_name = deparse(substitute(x)),
  call = rlang::caller_env()
) {
  if (length(x) == 0) {
    abort(
      "{.arg {arg_name}} cannot be empty",
      error_class = "halfmoon_empty_error",
      call = call
    )
  }
  invisible(x)
}

# Binary group validation
validate_binary_group <- function(
  group,
  arg_name = "group",
  call = rlang::caller_env()
) {
  levels <- unique(stats::na.omit(group))
  if (length(levels) != 2) {
    abort(
      "{.arg {arg_name}} must have exactly two levels, got {length(levels)}",
      error_class = "halfmoon_group_error",
      call = call
    )
  }
  invisible(levels)
}

# Data frame validation
validate_data_frame <- function(
  data,
  arg_name = ".data",
  call = rlang::caller_env()
) {
  if (!is.data.frame(data)) {
    abort(
      "{.arg {arg_name}} must be a data frame",
      error_class = "halfmoon_type_error",
      call = call
    )
  }
  invisible(data)
}

# Column existence validation
validate_column_exists <- function(
  data,
  column_name,
  arg_name = NULL,
  call = rlang::caller_env()
) {
  arg_name <- arg_name %||% column_name
  if (!column_name %in% names(data)) {
    abort(
      "Column {.code {column_name}} not found in {.arg {arg_name}}",
      error_class = "halfmoon_column_error",
      call = call
    )
  }
  invisible(TRUE)
}

# NA handling helpers
check_na_return <- function(..., na.rm = FALSE) {
  if (na.rm) {
    return(FALSE)
  }

  values <- list(...)
  any(vapply(values, anyNA, logical(1)))
}

# Filter indices based on NA values
filter_na_indices <- function(indices, data, weights = NULL, na.rm = FALSE) {
  if (!na.rm) {
    return(indices)
  }

  if (is.null(weights)) {
    indices[!is.na(data[indices])]
  } else {
    indices[!is.na(data[indices]) & !is.na(weights[indices])]
  }
}

# Categorical exposure validation
is_categorical_exposure <- function(group) {
  levels <- unique(stats::na.omit(group))
  length(levels) > 2
}
