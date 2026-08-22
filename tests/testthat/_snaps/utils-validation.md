# validate_weights rejects weights that are not a causal weight or numeric

    Code
      validate_weights(c("a", "b", "c"), 3)
    Condition <halfmoon_type_error>
      Error:
      ! `.weights` must be numeric, a causal weight object, or `NULL`

