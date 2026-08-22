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

# Weight type validation on its own, for the callers that apply their own
# policy to the values a weight column holds but still need the column to hold
# weights at all. Accepts numeric vectors and any causal weight object, which
# covers the psw objects from propensity and the bw objects from balancing.
validate_weight_type <- function(
  weights,
  arg_name = ".weights",
  allow_null = TRUE,
  call = rlang::caller_env()
) {
  if (is.numeric(weights) || causalgenerics::is_causal_wt(weights)) {
    return(invisible(weights))
  }

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

  validate_weight_type(
    weights,
    arg_name = arg_name,
    allow_null = allow_null,
    call = call
  )

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

# Binary group validation. Levels are the OBSERVED levels, so a factor
# carrying a declared level that no observation takes is still binary input.
validate_binary_group <- function(
  group,
  arg_name = "group",
  call = rlang::caller_env()
) {
  levels <- extract_group_levels(group, require_binary = FALSE, call = call)
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

#' Match one of a set of option strings
#'
#' Takes the first choice when the argument still holds its default vector, as
#' `match.arg()` does, and otherwise requires a single value naming a choice.
#' @noRd
match_option <- function(
  value,
  choices,
  arg,
  call = rlang::caller_env()
) {
  if (identical(value, choices)) {
    return(choices[[1]])
  }

  if (
    !is.character(value) ||
      length(value) != 1 ||
      is.na(value) ||
      !value %in% choices
  ) {
    abort(
      "{.arg {arg}} must be one of: {.val {choices}}",
      error_class = "halfmoon_arg_error",
      call = call
    )
  }

  value
}

#' Refuse a weight method that would collide with the unweighted rows
#'
#' Every `check_*()` result labels its unweighted rows `"observed"`. A weight
#' column of that name, or a selection that renames one to it, would otherwise
#' produce a second set of unweighted rows under the same label rather than the
#' weighted results the caller asked for.
#' @noRd
validate_method_labels <- function(
  method_names,
  call = rlang::caller_env()
) {
  if ("observed" %in% method_names) {
    abort(
      c(
        "{.arg .weights} cannot select a method named {.val observed}.",
        i = "{.val observed} labels the unweighted rows of the result.",
        i = "Rename the selection, as in {.code .weights = c(unweighted = observed)}."
      ),
      error_class = "halfmoon_arg_error",
      call = call
    )
  }

  invisible(method_names)
}

#' Require a model response that a calibration curve can read as an event
#'
#' Only observed values count, so a factor that declares levels no observation
#' takes is still binary input.
#' @noRd
validate_binary_response <- function(
  response,
  arg_name = "x",
  call = rlang::caller_env()
) {
  observed <- if (is.factor(response)) {
    levels(droplevels(response))
  } else {
    sort(unique(stats::na.omit(response)))
  }

  if (length(observed) != 2) {
    abort(
      "The response of {.arg {arg_name}} must take exactly two values, got {length(observed)}",
      error_class = "halfmoon_type_error",
      call = call
    )
  }

  invisible(response)
}

#' Refuse data that holds nothing to compute on
#'
#' A selection evaluated against an empty frame fails inside tidyselect, which
#' reports a missing column rather than the empty data that is the real problem,
#' so the check runs before any selection does.
#' @noRd
validate_data_not_empty <- function(
  data,
  arg_name = ".data",
  call = rlang::caller_env()
) {
  if (ncol(data) == 0 || nrow(data) == 0) {
    abort(
      "{.arg {arg_name}} must have at least one row and one column",
      error_class = "halfmoon_empty_error",
      call = call
    )
  }

  invisible(data)
}
