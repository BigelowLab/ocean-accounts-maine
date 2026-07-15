#' Retrieve the root data path
#' 
#' @export
#' @param filename chr the name where the root data path is stored (as a yaml)
#' @return file path to the data directory
root_data_path = function(){
  options("oame")$oame$data_path
}

#' Retrieve and/or build a data path
#' 
#' @export
#' @param ... chr, file path segments ala [base::file.path()]. These must come before
#'  the `root` argument.
#' @param root chr the root data path
#' @return a complete path specification
oame_path = function(...,
                     root = root_data_path()){
  file.path(root, ...)
}
