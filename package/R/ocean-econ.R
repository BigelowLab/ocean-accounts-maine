#' Plots ocean economy sectors over time for different variables
#' 
#' @export
#' @param plot_data tibble of filtered data for a specific region of Maine, or the whole state
#' @param y_var character string variable for y axis
#' @returns a ggplot
plot_ocean_economy = function(plot_data, y_var = "gdp") {
  ylabel <- case_when(
    y_var == "gdp" ~ "GDP ($ Billion)",
    y_var == "rgdp" ~ "Real GDP ($ Billion)",
    y_var == "wages" ~ "Wages ($ Billion)",
    y_var == "employment" ~ "Employment",
    y_var == "establishments" ~ "Establishments"
  )
  
  ggplot2::ggplot(data = plot_data, ggplot2::aes(x=year, y=!!ensym(y_var), color=sector)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(ggplot2::aes(shape = sector)) +
    ggplot2::ylab(ylabel) +
    ggplot2::scale_x_continuous(breaks = seq(min(plot_data$year), max(plot_data$year))) +
    ggplot2::theme_bw()
}