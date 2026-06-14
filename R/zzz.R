# Null-coalescing operator. `%||%` is only part of base R since 4.4.0, but the
# package supports R (>= 4.2.0), so we define it internally to stay portable.
`%||%` <- function(x, y) if (is.null(x)) y else x

.onLoad <- function(libname, pkgname) {
  if (is.null(getOption("evolution.timeout"))) {
    options(evolution.timeout = 60)
  }
}

.onUnload <- function(libpath) {
  options(evolution.timeout = NULL)
}
