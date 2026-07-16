# Adds the "oame.config" list to options
# 
# Adapted from [R Packages (2e)](https://r-pkgs.org/code.html#sec-code-onLoad-onAttach)
.onLoad <- function(libname, pkgname) {

  options(oame = read_config())
  
  invisible()
}