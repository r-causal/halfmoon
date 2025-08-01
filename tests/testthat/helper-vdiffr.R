expect_doppelganger <- function(title, fig, ...) {
  testthat::skip_if_not_installed("vdiffr")
  suppress_calibration_warnings(
    vdiffr::expect_doppelganger(title, fig, ...)
  )
}
