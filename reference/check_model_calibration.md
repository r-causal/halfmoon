# Compute calibration data for binary outcomes

`check_model_calibration()` summarizes predicted probabilities and
observed outcomes, computing mean prediction, observed rate, counts, and
confidence intervals. Calibration represents the agreement between
predicted probabilities and observed outcomes. Supports multiple methods
for calibration assessment.

## Usage

``` r
check_model_calibration(
  data,
  .fitted,
  .exposure,
  .focal_level = NULL,
  method = c("breaks", "logistic", "windowed"),
  bins = 10,
  binning_method = c("equal_width", "quantile"),
  smooth = TRUE,
  conf_level = 0.95,
  window_size = 0.1,
  step_size = window_size/2,
  k = 10,
  na.rm = FALSE
)
```

## Arguments

- data:

  A data frame containing the data.

- .fitted:

  Column name of predicted probabilities (numeric between 0 and 1). Can
  be unquoted (e.g., `p`) or quoted (e.g., `"p"`).

- .exposure:

  Column name of treatment/exposure variable. Can be unquoted (e.g.,
  `g`) or quoted (e.g., `"g"`).

- .focal_level:

  The level of the outcome variable to consider as the treatment/event.
  If `NULL` (default), uses the last level for factors or the maximum
  value for numeric variables.

- method:

  Character; calibration method. One of: "breaks", "logistic", or
  "windowed".

- bins:

  Integer \> 1; number of bins for the "breaks" method.

- binning_method:

  "equal_width" or "quantile" for bin creation (breaks method only).

- smooth:

  Logical; for "logistic" method, use GAM smoothing via the mgcv
  package.

- conf_level:

  Numeric in (0,1); confidence level for CIs (default = 0.95).

- window_size:

  Numeric; size of each window for "windowed" method.

- step_size:

  Numeric; distance between window centers for "windowed" method.

- k:

  Integer; the basis dimension for GAM smoothing when method =
  "logistic" and smooth = `TRUE.` Default is 10.

- na.rm:

  Logical; if `TRUE`, drop `NA` values before summarizing.

## Value

A tibble with columns:

- For "breaks" method:

  - `.bin`: integer bin index

  - `predicted_rate`: mean predicted probability in bin

  - `observed_rate`: observed treatment rate in bin

  - `count`: number of observations in bin

  - `lower`: lower bound of CI for `observed_rate`

  - `upper`: upper bound of CI for `observed_rate`

- For "logistic" and "windowed" methods:

  - `predicted_rate`: predicted probability values

  - `observed_rate`: calibrated outcome rate

  - `lower`: lower bound of CI

  - `upper`: upper bound of CI

## Examples

``` r
# Using the included `nhefs_weights` dataset
# `.fitted` contains propensity scores, and `qsmk` is the treatment variable
check_model_calibration(nhefs_weights, .fitted, qsmk)
#> Warning: Small sample sizes or extreme proportions detected in bins 9, 10 (n = 8, 3).
#> Confidence intervals may be unreliable. Consider using fewer bins or a
#> different calibration method.
#> # A tibble: 10 × 6
#>     .bin predicted_rate observed_rate count  lower upper
#>    <int>          <dbl>         <dbl> <int>  <dbl> <dbl>
#>  1     1         0.0971        0.0649   154 0.0333 0.119
#>  2     2         0.162         0.166    355 0.130  0.210
#>  3     3         0.230         0.254    445 0.215  0.298
#>  4     4         0.302         0.294    293 0.243  0.350
#>  5     5         0.372         0.368    155 0.293  0.449
#>  6     6         0.443         0.372     86 0.272  0.484
#>  7     7         0.516         0.511     45 0.360  0.661
#>  8     8         0.591         0.773     22 0.542  0.913
#>  9     9         0.648         0.375      8 0.102  0.741
#> 10    10         0.738         1          3 1      1    

# Logistic method with smoothing
check_model_calibration(nhefs_weights, .fitted, qsmk, method = "logistic")
#> # A tibble: 100 × 4
#>    predicted_rate observed_rate     lower     upper
#>             <dbl>     <dbl[1d]> <dbl[1d]> <dbl[1d]>
#>  1         0.0510        0.0651    0.0363     0.114
#>  2         0.0583        0.0694    0.0404     0.117
#>  3         0.0657        0.0740    0.0448     0.120
#>  4         0.0730        0.0789    0.0497     0.123
#>  5         0.0803        0.0840    0.0549     0.126
#>  6         0.0877        0.0894    0.0606     0.130
#>  7         0.0950        0.0952    0.0666     0.134
#>  8         0.102         0.101     0.0730     0.139
#>  9         0.110         0.108     0.0797     0.144
#> 10         0.117         0.114     0.0868     0.149
#> # ℹ 90 more rows

# Windowed method
check_model_calibration(nhefs_weights, .fitted, qsmk, method = "windowed")
#> Warning: Small sample sizes or extreme proportions detected in windows centered at 0.7,
#> 0.75, 0.8 (n = 5, 3, 1). Confidence intervals may be unreliable. Consider using
#> a larger window size or a different calibration method.
#> # A tibble: 16 × 4
#>    predicted_rate observed_rate  lower upper
#>             <dbl>         <dbl>  <dbl> <dbl>
#>  1           0.05        0.0506 0.0163 0.131
#>  2           0.1         0.106  0.0731 0.152
#>  3           0.15        0.156  0.124  0.193
#>  4           0.2         0.212  0.180  0.248
#>  5           0.25        0.258  0.223  0.296
#>  6           0.3         0.290  0.248  0.337
#>  7           0.35        0.327  0.274  0.386
#>  8           0.4         0.370  0.301  0.444
#>  9           0.45        0.420  0.331  0.514
#> 10           0.5         0.467  0.352  0.585
#> 11           0.55        0.562  0.413  0.702
#> 12           0.6         0.724  0.525  0.866
#> 13           0.65        0.625  0.359  0.837
#> 14           0.7         0.6    0.170  0.927
#> 15           0.75        1      1      1    
#> 16           0.8         1      1      1    
```
