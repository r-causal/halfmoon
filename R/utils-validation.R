# Validation helper functions for the halfmoon package

# Numeric validation
validate_numeric <- function(x, arg_name = deparse(substitute(x))) {
  if (!is.numeric(x)) {
    abort("{.arg {arg_name}} must be numeric, got {.cls {class(x)[1]}}")
  }
  invisible(x)
}

# Weight validation
validate_weights <- function(weights, n, arg_name = "weights") {
  if (is.null(weights)) return(invisible(weights))

  if (!is.numeric(weights)) {
    abort("{.arg {arg_name}} must be numeric or {.code NULL}")
  }
  if (length(weights) != n) {
    abort("{.arg {arg_name}} must have length {n}, got {length(weights)}")
  }
  if (any(vctrs::vec_data(weights) < 0, na.rm = TRUE)) {
    abort("{.arg {arg_name}} cannot contain negative values")
  }
  invisible(weights)
}

# Length validation
validate_equal_length <- function(x, y, x_name = NULL, y_name = NULL) {
  x_name <- x_name %||% deparse(substitute(x))
  y_name <- y_name %||% deparse(substitute(y))

  x_len <- if (is.matrix(x) || is.data.frame(x)) nrow(x) else length(x)
  y_len <- if (is.matrix(y) || is.data.frame(y)) nrow(y) else length(y)

  if (x_len != y_len) {
    abort("{.arg {x_name}} and {.arg {y_name}} must have the same length")
  }
  invisible(TRUE)
}

# Non-empty validation
validate_not_empty <- function(x, arg_name = deparse(substitute(x))) {
  if (length(x) == 0) {
    abort("{.arg {arg_name}} cannot be empty")
  }
  invisible(x)
}

# Binary group validation
validate_binary_group <- function(group, arg_name = "group") {
  levels <- unique(stats::na.omit(group))
  if (length(levels) != 2) {
    abort(
      "{.arg {arg_name}} must have exactly two levels, got {length(levels)}"
    )
  }
  invisible(levels)
}

# Reference group validation
validate_reference_group <- function(
  reference_group,
  levels,
  arg_name = "reference_group"
) {
  if (!reference_group %in% levels) {
    abort(
      "{.arg {arg_name}} {.val {reference_group}} not found in grouping variable"
    )
  }
  invisible(reference_group)
}

# Data frame validation
validate_data_frame <- function(data, arg_name = ".data") {
  if (!is.data.frame(data)) {
    abort("{.arg {arg_name}} must be a data frame")
  }
  invisible(data)
}

# Column existence validation
validate_column_exists <- function(data, column_name, arg_name = NULL) {
  arg_name <- arg_name %||% column_name
  if (!column_name %in% names(data)) {
    abort("Column {.code {column_name}} not found in {.arg {arg_name}}")
  }
  invisible(TRUE)
}

# NA handling helpers
check_na_return <- function(..., na.rm = FALSE) {
  if (na.rm) return(FALSE)

  values <- list(...)
  any(vapply(values, function(x) any(is.na(x)), logical(1)))
}

# Filter indices based on NA values
filter_na_indices <- function(indices, data, weights = NULL, na.rm = FALSE) {
  if (!na.rm) return(indices)

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

# Get exposure type
get_exposure_type <- function(group) {
  levels <- unique(stats::na.omit(group))
  n_levels <- length(levels)

  if (n_levels == 2) {
    "binary"
  } else if (n_levels > 2) {
    "categorical"
  } else if (n_levels == 1) {
    abort("Group variable has only one level")
  } else {
    abort("Group variable has no non-missing values")
  }
}

# Validate exposure type
validate_exposure_type <- function(group, arg_name = "group") {
  exposure_type <- get_exposure_type(group)

  # For now, just return the type - validation happens in get_exposure_type
  invisible(exposure_type)
}
