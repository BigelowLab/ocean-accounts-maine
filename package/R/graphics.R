#' Plot a timeseries 
#'
#' 
#' @export
#' @param x data frame with columns "date" and "value"
#' @param title chr, the title for the plot
#' @param smooth chr or NULL, if not NULL then the smoothing type to use
#'   otherwise don't apply smoothing
#' @param facet logical, if TRUE facet by month
#' @return ggplot object
plot_monthly_timeseries = function(x = read_nao(),
                                   title = "NAO",
                                   facet = FALSE){
  
  x = dplyr::mutate(x, month = factor(.data$month, levels = month.abb))
  gg = ggplot2::ggplot(data = x) +
    ggplot2::geom_line(mapping = ggplot2::aes(x = date, y = value)) + 
    ggplot2::labs(title = title,
                  x = "Date",
                  y = "Index")
  if (!is.null(smooth)) gg = gg + ggplot2::geom_smooth(
                                    mapping = ggplot2::aes(x = date, y = value),
                                    formula = y ~ x,
                                    method = "loess") 
  if (facet) gg = gg + ggplot2::facet_wrap(~month) 
  return(gg)
}

#' @export
#' @rdname plot_monthly_timeseries
plot_monthly_climatology = function(x = read_nao(),
                                    title = "NAO"){

  lut = 1:12 |>
    rlang::set_names(month.abb)
  xlabels = function(x){month.abb[x]}
  x = dplyr::mutate(na.omit(x), 
                    imonth = lut[.data$month])
  recent_year = max(x$year)
  y = dplyr::filter(x, .data$year == recent_year)
  gg = ggplot2::ggplot(data = x |>
                         dplyr::group_by(.data$year),
                        mapping = ggplot2::aes(x = .data$imonth, 
                                               y = .data$value, 
                                               group = .data$year,
                                               color = .data$year)) +
    ggplot2::geom_path( ) +
    ggplot2::scale_color_viridis_c(direction = 1, alpha = 0.7, option = "viridis") + 
    ggplot2::scale_x_continuous(breaks = 1:12,
                                labels = xlabels) + 
    ggplot2::geom_path(data = y,
                       mapping = ggplot2::aes(x = .data$imonth, 
                                              y = .data$value),
                       col = "blue", linewidth = 2) + 
    ggplot2::labs(title = title,
                  x = "Month",
                  y = "Index",
                  )
  return(gg)
}

#' @export
#' @rdname plot_monthly_timeseries
plot_nao = function(x = read_nao(),
                    type = c("timeseries", "monthly", "climatology")[1]){
  switch(tolower(type[1]),
         "timeseries" = plot_monthly_timeseries(x,title = "NAO"),
         "monthly" = plot_monthly_timeseries(x, title = "NAO", facet = TRUE),
         "climatology" = plot_monthly_climatology(x, title = "NAO"),
         stop("type not known: ", type[1])
  )
}

#' @export
#' @rdname plot_monthly_timeseries
plot_amo = function(x = read_amo(),
                    type = c("timeseries", "monthly", "climatology")[1]){
  switch(tolower(type[1]),
         "timeseries" = plot_monthly_timeseries(x,title = "AMO"),
         "monthly" = plot_monthly_timeseries(x, title = "AMO", facet = TRUE),
         "climatology" = plot_monthly_climatology(x, title = "AMO"),
         stop("type not known: ", type[1])
  )
}