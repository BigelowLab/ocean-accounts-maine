


#' Build a map of the hurricane paths faceted by user-specified epoch
#' 
#' @export
#' @param x HURDAT sf object
#' @param color_by chr, one of "wind_sus_max", "duration", or "min_pres"
#' @param epoch num, the number of consecutive years to group by
#' @param counties spatial geometry of counties (maine)
#' @param coast spatial geometry of coastline (gulf of maine)
#' @return ggplot2 object
map_hurdat = function(x = read_hurdat(),
                      color_by = c("wind_max_sus", "duration", "min_pres")[1],
                      epoch = c(10, 25, 50, 100)[2],
                      counties = read_me_counties() |>
                        sf::st_geometry() |>
                        sf::st_crop(x),
                      coast = read_coast()){
  
  if (FALSE){
    devtools::load_all("package")
    library(ggplot2)
    library(colorspace)
    library(sf)
    x = read_hurdat()
    epoch = 50
    counties = read_me_counties() |>
      sf::st_crop(x)
    coast = read_coast()
  }
  
  code_epoch = function(y = c(1851, 1922, 1877, 2025),
                        epoch = c(10, 25, 50, 100)[3]){
    s = seq(from = 1850, to = 2050, by = epoch)
    s2 = seq(from = 1850 + epoch - 1, to = 2050 + epoch - 1, by = epoch)
    ix = findInterval(y, s)
    paste(s, s2, sep = "-")[ix]
  }
  
  x = x |>
    dplyr::mutate(epoch = format(.data$start, "%Y") |>
                    as.numeric() |>
                    code_epoch(epoch = epoch) )
  
  ggplot2::ggplot() + 
    ggplot2::geom_sf(data = x,
                     mapping = ggplot2::aes(color = .data[[color_by]]),
                     alpha = 1) + 
    colorspace::scale_color_continuous_sequential(palette = "Purples 3") + 
    ggplot2::geom_sf(data = coast, color = "orange") + 
    ggplot2::geom_sf(data = counties, color = "black", fill = NA) + 
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank()) + 
    ggplot2::labs(x = NULL, y = NULL, title = "Hurricane Data (HURDAT2)") +
    ggplot2::facet_wrap(~epoch)
}