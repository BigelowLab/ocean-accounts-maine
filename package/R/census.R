#' Plots county level census data for Maine coastal counties
#' 
#' @export
#' @param plot_data tibble of filtered data for a specific region of Maine, or the whole state
#' @param y_var character string variable for y axis
#' @returns a ggplot
plot_census = function(plot_data = oame::read_census(),
                       y_var = "population") {
  ylabel <- case_when(
    y_var == "population" ~ "Population",
    y_var == "housing" ~ "Housing Units",
    y_var == "med_home_value" ~ "Median Home Value ($)"
  )
  ggplot2::ggplot(data = plot_data, ggplot2::aes(x = year, y = !!ensym(y_var))) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::ylab(ylabel) +
    ggplot2::facet_wrap(facets = vars(name), scales="free") +
    ggplot2::theme_bw()
}



