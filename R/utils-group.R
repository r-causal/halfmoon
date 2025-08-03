# Group handling helper functions for the halfmoon package

# Extract and validate group levels
extract_group_levels <- function(group, require_binary = TRUE) {
  levels <- if (is.factor(group)) {
    levels(group)
  } else {
    group |>
      stats::na.omit() |>
      unique() |>
      sort()
  }

  if (require_binary && length(levels) != 2) {
    abort(
      "Group variable must have exactly two levels, got {length(levels)}"
    )
  }

  levels
}

# Determine reference group with consistent logic
determine_reference_group <- function(group, reference_group = NULL) {
  levels <- extract_group_levels(group, require_binary = FALSE)

  if (is.null(reference_group)) {
    # Default to first level
    return(levels[1])
  }

  # First check if the value exists in the group levels (exact match)
  if (reference_group %in% levels) {
    return(reference_group)
  }

  # If not in levels and is numeric, treat as index
  if (is.numeric(reference_group) && length(reference_group) == 1) {
    if (reference_group > length(levels) || reference_group < 1) {
      abort("Reference group index {reference_group} out of bounds")
    }
    return(levels[reference_group])
  }

  # Otherwise, it's an invalid reference group
  abort(
    "{.arg reference_group} {.val {reference_group}} not found in grouping variable"
  )
}

# Create treatment indicator
create_treatment_indicator <- function(group, treatment_level = NULL) {
  unique_levels <- unique(group[!is.na(group)])

  # Handle empty groups
  if (length(unique_levels) == 0) {
    return(integer(length(group)))
  }

  if (is.null(treatment_level)) {
    if (is.factor(group)) {
      treatment_level <- levels(group)[length(levels(group))]
    } else {
      treatment_level <- max(unique_levels, na.rm = TRUE)
    }
  }

  # Handle both factor and non-factor variables
  if (is.factor(group)) {
    as.integer(as.character(group) == as.character(treatment_level))
  } else {
    as.integer(group == treatment_level)
  }
}

# Split data by group
split_by_group <- function(data, group, reference_group = NULL) {
  levels <- extract_group_levels(group, require_binary = TRUE)
  ref <- determine_reference_group(group, reference_group)

  list(
    reference = which(group == ref),
    comparison = which(group != ref),
    reference_level = ref,
    comparison_level = setdiff(levels, ref)[1]
  )
}
