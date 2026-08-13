# Adds the "oame.config" list to options
# 
# Adapted from [R Packages (2e)](https://r-pkgs.org/code.html#sec-code-onLoad-onAttach)
.onLoad <- function(libname, pkgname) {

  if (file.exists("~/.oame")) {
    options(oame = read_config())
  } else {
    options(oame = list(data_path = "/mnt/ecocast/corecode/R/oceanaccounts/oame/data"))
  }
  
  invisible()
}