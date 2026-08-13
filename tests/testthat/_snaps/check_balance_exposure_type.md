# check_balance rejects discrete metrics for a continuous exposure

    Code
      expr
    Condition <halfmoon_metric_type_error>
      Error in `check_balance()`:
      ! Metric "smd" cannot be computed for a "continuous" exposure.
      i Metrics for a "continuous" exposure: "correlation" and "energy".
      i Set `exposure_type` to read `.exposure` as another type.

# check_balance rejects correlation for a binary exposure

    Code
      expr
    Condition <halfmoon_metric_type_error>
      Error in `check_balance()`:
      ! Metric "correlation" cannot be computed for a "binary" exposure.
      i Metrics for a "binary" exposure: "smd", "vr", "ks", and "energy".
      i Set `exposure_type` to read `.exposure` as another type.

# check_balance rejects correlation for a categorical exposure

    Code
      expr
    Condition <halfmoon_metric_type_error>
      Error in `check_balance()`:
      ! Metric "correlation" cannot be computed for a "categorical" exposure.
      i Metrics for a "categorical" exposure: "smd", "vr", "ks", and "energy".
      i Set `exposure_type` to read `.exposure` as another type.

# check_balance requires a numeric exposure treated as continuous

    Code
      expr
    Condition <halfmoon_type_error>
      Error in `check_balance()`:
      ! Exposure variable must be numeric when treated as continuous

---

    Code
      expr
    Condition <halfmoon_type_error>
      Error in `check_balance()`:
      ! Exposure variable must be numeric when treated as continuous

# check_balance still rejects unknown metric names

    Code
      expr
    Condition <halfmoon_arg_error>
      Error in `check_balance()`:
      ! Invalid metric: "invalid"
      i Available metrics: "smd", "vr", "ks", "correlation", and "energy"

