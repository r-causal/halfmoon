# Plot ROC AUC Values for Balance Assessment

Creates a visualization of AUC values from weighted ROC analysis. Values
near 0.5 indicate good balance.

## Usage

``` r
plot_model_auc(
  .data,
  ref_line = TRUE,
  ref_color = "red",
  point_size = 3,
  point_shape = 19
)
```

## Arguments

- .data:

  Output from
  [`check_model_auc()`](https://r-causal.github.io/halfmoon/reference/check_model_auc.md).

- ref_line:

  Show reference line at AUC = 0.5? Default is TRUE.

- ref_color:

  Color for reference line. Default is "red".

- point_size:

  Size of the points. Default is 3.

- point_shape:

  Shape of the points. Default is 19 (filled circle).

## Value

A ggplot2 object.

## Examples

``` r
# Compute AUC values
auc_data <- check_model_auc(
  nhefs_weights,
  qsmk,
  .fitted,
  c(w_ate, w_att)
)

# Create plot
plot_model_auc(auc_data)

```
