# Adds the "oame.config" list to options
# 
# Adapted from [R Packages (2e)](https://r-pkgs.org/code.html#sec-code-onLoad-onAttach)
.onLoad <- function(libname, pkgname) {
  #op <- options()
  #op.oame <- list(
  #  #dplyr.show_progress = TRUE
  #  config = read_config()
  #)
  #toset <- !(names(op.oame) %in% names(op))
  #if (any(toset)) options(op.oame[toset])
  
  options(oame = read_config())
  
  invisible()
}