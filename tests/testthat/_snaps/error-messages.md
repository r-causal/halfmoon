# error messages show user-facing function names

    Code
      plot_mirror_distributions(nhefs_weights, age, alcoholfreq_cat, reference_group = "invalid")
    Condition
      Error in `plot_mirror_distributions()`:
      ! `reference_group` "invalid" not found in grouping variable

---

    Reference group index 10 out of bounds

---

    Argument `.var` is required

---

    Column `missing_column` not found in `.var`

---

    `treatment_level` 'invalid' not found in `.group` levels: "0" and "1"

---

    Argument `treatment` is required

---

    Can't convert a call to a string.

---

    The treatment variable 'qsmk' should not be included in the outcome model formula.

# validation errors show correct function context

    Column `not_numeric` not found in `data`

---

    Can't select columns with `list(bad_wts = bad_weights)`.
    x `list(bad_wts = bad_weights)` must be numeric or character, not a list.

---

    Can't select columns that don't exist.
    x Column `qsmk` doesn't exist.

