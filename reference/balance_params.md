# Parameter Documentation for Balance Functions

This function exists solely to document parameters shared across balance
functions.

## Arguments

- .covariate:

  A numeric vector containing the covariate values to compare.

- .exposure:

  A vector (factor or numeric) indicating group membership. Must have
  exactly two unique levels.

- .weights:

  An optional numeric vector of case weights. If provided, must have the
  same length as other input vectors. All weights must be non-negative.

- .reference_level:

  The reference group level for comparisons. Can be either a group level
  value or a numeric index. If `NULL` (default), uses the first level.

- na.rm:

  A logical value indicating whether to remove missing values before
  computation. If `FALSE` (default), missing values in the input will
  produce `NA` in the output.
