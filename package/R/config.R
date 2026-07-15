#' Read the configuration file
#' 
#' @export
#' @param filename chr, the path specification for the config file
#' @return a configuration list
read_config = function(filename = "~/.oame"){
  yaml::read_yaml(filename)
}
