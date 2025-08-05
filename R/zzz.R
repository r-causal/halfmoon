.onLoad <- function(libname, pkgname) {
  # Ensure propensity's namespace is loaded so its S3 methods are registered
  # This is needed for psw objects in nhefs_weights to work properly without
  # users having to explicitly load propensity
  loadNamespace("propensity")

  invisible()
}
