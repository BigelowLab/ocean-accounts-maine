#' Place ME research institutions on an interactive leaflet map
#' 
#' @export
#' @param x tibble of institutions with lat/lon locations
#' @returns a leaflet map
map_labs = function(x) {
  #pal <- colorFactor(c("navy", "red", "forestgreen", "orange"), domain = c("Academic", "Investment", "Non-Profit", "State"))
  
  leaflet::leaflet(data=x) |>
    leaflet::addTiles() |>
    leaflet::addMarkers(lng=~long, 
                        lat=~lat,
                        #color=~pal(category),
                        #fillColor=~pal(category),
                        popup = ~name) |>
    leaflet::setView(-69.58207, 43.86031, zoom = 7)
}