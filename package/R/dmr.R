#' Prep landings data form county analyses
#' 
#' @export
#' @param x DMR landings table
#' @return reduced DMR table to have valid county names
prep_dmr_landings_county = function(x = read_dmr_landings("modern")){
  x |>
    dplyr::filter(!(.data$county %in% c("UK", "Not-Specified", "ME")),
                  !is.na(.data$species))
}


#' Aggregate by DMR landings data by county, year and species
#' 
#' @export
#' @param x DMR landings data
#' @param collapse chr, "none" (default) or "year" to aggregate years etc
#' @return summary table
aggregate_dmr_landings_county = function(x = read_dmr_landings("modern"),
                                         collapse = "none"){
  
  r = switch(tolower(collapse[1]),
            "year" = prep_dmr_landings_county(x) |>
              dplyr::group_by(.data$county, .data$species),
             "none" = prep_dmr_landings_county(x) |>
               dplyr::group_by(.data$county, .data$year, .data$species),
            stop("collapse value not known:", collapse))
  
   r |>   
     dplyr::summarize(weight = sum(weight, na.rm = TRUE),
                      value = sum(value, na.rm = TRUE),
                      trip_n = sum(trip_n, na.rm = TRUE),
                      harv_n = sum(harv_n, na.rm = TRUE),
                      town_n = dplyr::n(),
                      .groups = "drop")
  
}


#' Make a chloropleth map by county for one or more species and 
#' and one or more years.
#' 
#' @export
#' @param x table of DMR landings
#' @param spp chr, one or more species
#' @param years chr or num, "recent" for the most recent year for the species,
#'   or specify your own year.  If you specify multiple years the data is 
#'   aggregated.
#' @param varname chr the name of the variable to map
#' @param counties sf table with "county" attribute and in 3857 CRS
#' @param style chr, one of "plain" or "cartogram"
#' @return map plot object
map_species_by_county = function(x = read_dmr_landings("modern"),
                                 spp = "Clam Soft",
                                 years = "recent",
                                 varname = "trip_n",
                                 counties = read_me_counties(crs = 3857),
                                 style = c("plain", "cartogram")[1]){
  if (FALSE){
    x = read_dmr_landings("modern")
    spp = "Clam Soft"
    years = "recent"
    counties = read_me_counties(crs = 3857)
    varname = "trip_n"
    style = "plain"
  }
  x = prep_dmr_landings_county(x) |>
    dplyr::ungroup() |>
    dplyr::filter(.data$species %in% spp) 
  
  if (inherits(years, "character")){
    x = switch(tolower(years),
               "recent" =  x |> 
                      dplyr::slice_max(.data$year) |>
                        aggregate_dmr_landings_county(),
               "all" = aggregate_dmr_landings_county(x, collapse = "year"),
                x |>
                 dplyr::filter(.data$year %in% years) |>
                 aggregate_dmr_landings_county(collapse = "year"))
    years = switch(tolower(years[1]),
        "recent" = as.character(max(x$year)),
        "all" = {
          r = range(x$year)
          paste(r[1], r[2], sep = " - ")
        },
        {
          r = range(as.numeric(years))
          paste(r[1], r[2], sep = " - ")
        })
  } else {
    collapse = if(length(years) > 1) "year" else "none"
    x = x |>
      dplyr::filter(.data$year %in% years) |>
      aggregate_dmr_landings_county(collapse = collapse)
    years = if(length(years) > 1) {
        as.character(years) 
      } else { 
        r = range(years)
        years = paste(r[1], r[2], sep = " - ")
      }
  }
  
  x = dplyr::left_join(counties, x, by = "county")
  
  
  
  gg = ggplot2::ggplot(data = counties) + 
    ggplot2::geom_sf(color = "grey", fill = NA, alpha = 0) 
  
  if (tolower(style[1]) == "cartogram"){
    carto = suppressWarnings(cartogram::cartogram_cont(x, weight = varname))
    gg = gg + 
      ggplot2::geom_sf(data = carto[varname],
                       mapping = ggplot2::aes(fill = .data[[varname]])) 
  } else{
      
    gg = gg + 
      ggplot2::geom_sf(data = x[varname],
                       mapping = ggplot2::aes(fill = .data[[varname]])) 
    
  }  
  
  gg + 
    ggplot2::scale_fill_viridis_c(direction = 1)  + 
    ggplot2::labs(title = spp,
                  subtitle = sprintf("year(s): %s", paste(years, collapse = " ")))
}



plot_dmr_county = function(x = "year", 
                           y = "trip_n",
                           spp = "Clam Soft",
                           data = read_dmr_landings("modern")){
  
  if (FALSE){
    x = "year"
    y = "trip_n"
    spp = "Clam Soft"
    data = read_dmr_landings("modern")
  }
  
  data = prep_dmr_landings_county(data)|>
    dplyr::filter(.data$species %in% spp)
  if (length(spp) > 1) {
    data = data |>
             aggregate_dmr_landings_county(collapse = "species")
  } else {
    data = data |>
      aggregate_dmr_landings_county()
  }
  
  
  
}