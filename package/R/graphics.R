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
  epochs = function(x){
    x = round(x/10) * 10
    p = pretty(x)
    cut(x, p, labels = p[seq_len(length(p)-1)])
  }
  
  x = dplyr::mutate(na.omit(x), 
                    month = factor(.data$month, levels = month.abb),
                    epoch = epochs(.data$year))
  n = length(levels(x$epoch))
  gg = ggplot2::ggplot(data = x |>
                    dplyr::group_by(.data$month)) +
    ggplot2::geom_violin(
      mapping = ggplot2::aes(x = month, y = value),
      draw_quantiles = c(0.25, 0.5, 0.75),
      quantile.linewidth = c(0.2, 1, 0.2)) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(x = month, y = value, color = epoch),
      position = ggplot2::position_jitter(width = 0.1),
      alpha = 0.3) + 
    ggplot2::scale_color_brewer(type = "qual", palette = "Dark2") + 
    ggplot2::labs(title = title,
                  x = "Month",
                  y = "Index")
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