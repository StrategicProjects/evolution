.onLoad <- function(libname, pkgname){
  if (is.null(getOption("evolution.timeout"))) options(evolution.timeout = 60)
}
.onUnload <- function(libpath){ options(evolution.timeout = NULL) }
