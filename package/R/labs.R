#' Place ME research institutions on an interactive leaflet map
#' 
#' @export
#' @param x tibble of institutions with lat/lon locations
#' @returns a leaflet map
map_labs = function(x) {
  leaflet::leaflet(data=x) |>
    leaflet::addTiles() |>
    leaflet::addMarkers(lng=~long, 
                        lat=~lat,
                        popup = ~name)
}