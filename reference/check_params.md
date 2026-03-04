# Parameter Documentation for Check Functions

This function exists solely to document parameters shared across check
functions.

## Arguments

- .data:

  A data frame containing the variables to analyze.

- .vars:

  Variables for which to calculate metrics. Can be unquoted variable
  names, a character vector, or a tidyselect expression.

- .exposure:

  Grouping variable, e.g., treatment or exposure group.

- .weights:

  Optional weighting variables. Can be unquoted variable names, a
  character vector, or NULL. Multiple weights can be provided to compare
  different weighting schemes.

- include_observed:

  Logical. If using `.weights`, also calculate observed (unweighted)
  metrics? Defaults to TRUE.
