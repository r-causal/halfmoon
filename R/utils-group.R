# Group handling helper functions for the halfmoon package

# Drop declared factor levels that no observation takes, so that a factor and
# the values it actually holds describe the same set of groups
drop_unused_levels <- function(group) {
  if (is.factor(group)) droplevels(group) else group
}

# Extract and validate group levels. Levels are the OBSERVED levels: declared
# order for a factor, sorted order otherwise.
extract_group_levels <- function(
  group,
  require_binary = TRUE,
  call = rlang::caller_env()
) {
  levels <- if (is.factor(group)) {
    # `droplevels()` would rebuild the whole factor just to read its used
    # levels back off; counting the codes answers the same question
    levels(group)[tabulate(group, nlevels(group)) > 0]
  } else {
    group |>
      stats::na.omit() |>
      unique() |>
      sort()
  }

  if (require_binary && length(levels) != 2) {
    abort(
      "Exposure variable must have exactly two levels, got {length(levels)}",
      error_class = "halfmoon_group_error",
      call = call
    )
  }

  levels
}

# Determine reference group with consistent logic
determine_reference_group <- function(
  group,
  reference_group = NULL,
  call = rlang::caller_env()
) {
  levels <- extract_group_levels(group, require_binary = FALSE, call = call)

  if (is.null(reference_group)) {
    # Default to first level
    return(levels[1])
  }

  validate_reference_group_scalar(reference_group, call = call)

  # First check if the value exists in the group levels (exact match)
  if (reference_group %in% levels) {
    return(reference_group)
  }

  # If not in levels and is numeric, treat as index
  if (is.numeric(reference_group)) {
    validate_reference_group_index(reference_group, length(levels), call = call)
    return(levels[reference_group])
  }

  # Otherwise, it's an invalid reference group
  abort(
    "{.arg .reference_level} {.val {reference_group}} not found in grouping variable",
    error_class = "halfmoon_reference_error",
    call = call
  )
}

# A reference level names a single group, either by value or by position
validate_reference_group_scalar <- function(
  reference_group,
  call = rlang::caller_env()
) {
  if (length(reference_group) != 1) {
    abort(
      "{.arg .reference_level} must be length 1, not length {length(reference_group)}",
      error_class = "halfmoon_arg_error",
      call = call
    )
  }

  if (is.na(reference_group)) {
    abort(
      "{.arg .reference_level} cannot be {.code NA}",
      error_class = "halfmoon_arg_error",
      call = call
    )
  }

  invisible(reference_group)
}

# A numeric reference level that does not match a level value is a position, so
# it has to be a whole number in range. `as.integer()` is not used here because
# it warns and returns NA above `.Machine$integer.max`.
validate_reference_group_index <- function(
  reference_group,
  n_levels,
  call = rlang::caller_env()
) {
  if (reference_group > n_levels || reference_group < 1) {
    abort(
      ".reference_level index {reference_group} out of bounds",
      error_class = "halfmoon_range_error",
      call = call
    )
  }

  if (reference_group %% 1 != 0) {
    abort(
      "{.arg .reference_level} index must be a whole number, not {reference_group}",
      error_class = "halfmoon_arg_error",
      call = call
    )
  }

  invisible(reference_group)
}

# Create treatment indicator
create_treatment_indicator <- function(group, .focal_level = NULL) {
  unique_levels <- unique(group[!is.na(group)])

  # Handle empty groups
  if (length(unique_levels) == 0) {
    return(integer(length(group)))
  }

  if (is.null(.focal_level)) {
    if (is.factor(group)) {
      .focal_level <- levels(group)[nlevels(group)]
    } else {
      .focal_level <- max(unique_levels, na.rm = TRUE)
    }
  }

  # Handle both factor and non-factor variables
  if (is.factor(group)) {
    as.integer(as.character(group) == as.character(.focal_level))
  } else {
    as.integer(group == .focal_level)
  }
}

# Split data by group
split_by_group <- function(
  data,
  group,
  reference_group = NULL,
  call = rlang::caller_env()
) {
  levels <- extract_group_levels(group, require_binary = TRUE, call = call)
  ref <- determine_reference_group(group, reference_group, call = call)

  list(
    reference = which(group == ref),
    comparison = which(group != ref),
    reference_level = ref,
    comparison_level = setdiff(levels, ref)[1]
  )
}
