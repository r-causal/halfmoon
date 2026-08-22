# bal_ess reports its own name and argument for invalid weights

    Code
      bal_ess(NULL)
    Condition <halfmoon_type_error>
      Error in `bal_ess()`:
      ! `.weights` must be numeric or a causal weight object, not `NULL`

---

    Code
      bal_ess(c("a", "b"))
    Condition <halfmoon_type_error>
      Error in `bal_ess()`:
      ! `.weights` must be numeric or a causal weight object

---

    Code
      bal_ess(c(1, -1))
    Condition <halfmoon_range_error>
      Error in `bal_ess()`:
      ! `.weights` cannot contain negative values

