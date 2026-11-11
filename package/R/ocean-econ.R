#' Plots ocean economy sectors over time for different variables
#' 
#' @export
#' @param plot_data tibble of filtered data for a specific region of Maine, or the whole state
#' @param y_var character string variable for y axis
#' @returns a ggplot
plot_ocean_economy = function(plot_data, y_var = "gdp") {
  ylabel <- case_when(
    y_var == "gdp" ~ "GDP ($)",
    y_var == "rgdp" ~ "Real GDP ($)",
    y_var == "wages" ~ "Wages ($)",
    y_var == "employment" ~ "Employment",
    y_var == "establishments" ~ "Establishments"
  )
  
  ggplot2::ggplot(data = plot_data, ggplot2::aes(x=year, y=!!ensym(y_var), color=sector)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(ggplot2::aes(shape = sector)) +
    ggplot2::ylab(ylabel) +
    ggplot2::scale_x_continuous(breaks = seq(min(plot_data$year), max(plot_data$year))) +
    ggplot2::theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size = 20),
          axis.text = element_text(size = 16))
}


#' Plots ocean economy sectors over time for different variables - interactive
#' 
#' @export
#' @param plot_data tibble of filtered data for a specific region of Maine, or the whole state
#' @param y_var character string variable for y axis
#' @returns an interactive plotly object
plot_ocean_economy_i = function(plot_data, y_var = "gdp") {
  ylabel <- case_when(
    y_var == "gdp" ~ "GDP ($)",
    y_var == "rgdp" ~ "Real GDP ($)",
    y_var == "wages" ~ "Wages ($)",
    y_var == "employment" ~ "Employment",
    y_var == "establishments" ~ "Establishments"
  )
  
  plotly::plot_ly(plot_data, x = ~year, y=~get(y_var), color=~sector, connectgaps=TRUE) |>
    plotly::add_trace(mode="lines+markers") |>
    plotly::layout(hovermode = "x unified",
                   xaxis = list(title = ""),
                   yaxis = list(title = ylabel))
  
}
