# geom_mirrored_histogram errors correctly

    Computation failed in `stat_mirror_count()`.
    Caused by error in `compute_group()`:
    ! No group detected.
    * Do you need to use `aes(group = ...)` with your grouping variable?

# geom_mirror_histogram names the geom and the group count

    Code
      ggplot2::ggplot_build(p)
    Condition <rlang_error>
      Error in `ggplot2::geom_histogram()`:
      ! Problem while computing stat.
      i Error occurred in the 1st layer.
      Caused by error in `setup_data()`:
      ! `geom_mirror_histogram()` draws at most two groups per panel, and a panel here has 6.
      i One group is drawn above the axis and one below, so there is no partial plot to draw.

