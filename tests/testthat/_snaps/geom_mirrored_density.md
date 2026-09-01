# geom_mirrored_density errors with 3+ groups

    Code
      ggplot_build(edu_group)
    Condition <rlang_error>
      Error in `geom_mirror_density()`:
      ! Problem while computing stat.
      i Error occurred in the 1st layer.
      Caused by error in `setup_data()`:
      ! `geom_mirror_density()` draws at most two groups per panel, and a panel here has 5.
      i One group is drawn above the axis and one below, so there is no partial plot to draw.

