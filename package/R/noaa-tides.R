#' Retrieve a table of select stations
#' 
#' @export
#' @param root chr, the root URL to access data
#' @return data frame with "name", "id" and "url"
tide_stations = function(
    root = "https://tidesandcurrents.noaa.gov/sltrends/data"){
  
  dplyr::tibble(
    name = c("Eastport", "Cutler", "Bar Harbor", "Portland", "Seavey Island"),
    id = c(8410140, 8411250, 8413320, 8418150, 8419870),
    url = file.path(root, sprintf("%0.4i_meantrend.csv", id)) )
}

#' Fetch tide data
#' 
#' @export
#' @param x data frame of tide stations
#' @param path chr, the root data path
#' @return the input with an added "filename" column
fetch_tides = function(x = tide_stations(),
                       path = oame_path("NOAA", "tides")){
  x |>
    dplyr::rowwise() |>
    dplyr::group_map(
      function(row, key){
        ofile = file.path(path, basename(row$url))
        ok = download.file(row$url, 
                           ofile,
                           mode = "wb")
        row |> 
          dplyr::mutate(filename = ofile)
      }
    ) |>
    dplyr::bind_rows()
}


#' Read tide data
#' 
#' @export
#' @param x data frame of tide stations identifiers
#' @param path chr, the root data path
#' @param drop chr, the names of stations to drop, "none" to drop none.
#' @return data frame of tide station data
read_tide = function(x = tide_stations(),
                     path = oame_path("NOAA", "tides"),
                     drop = c("Cutler", "none")[1]){
  
  x |>
    dplyr::rowwise() |>
    dplyr::group_map(
      function(row, key){
        filename = file.path(path, sprintf("%i_meantrend.csv", row$id))
        cnames = c("year", "month",
                      "MSL", "Linear_Trend", 
                      "High_Conf", "Low_Conf",
                      "empty")
        readr::read_csv(filename,
                        skip = 6,
                        col_names = cnames,
                        col_types = rep("n", length(cnames)) |> 
                                      paste(collapse = "")) |>
          dplyr::select(-dplyr::any_of("empty")) |>
          dplyr::mutate(station = row$name, 
                        date = as.Date(sprintf("%0.4i-%0.2i-01", .data$year, .data$month), 
                                       format = "%Y-%m-%d"),
                        .before = 1)
      }) |>
    dplyr::bind_rows() |>
    dplyr::filter(!.data$station %in% drop)
}

#' Plot monthly tide and tide climatology
#' 
#' @export
#' @param x data frame of tide data
#' @return station faceted ggplot object
plot_tide_climatology = function(x = read_tide()){
  
  x = dplyr::group_by(x, .data$station)
  y = x |> 
    dplyr::group_map(
      function(grp, key){
        mx = max(grp$year)
        dplyr::filter(grp, year == mx)
      },
      .keep = TRUE
    ) |>
    dplyr::bind_rows()
  x = dplyr::group_by(x, .data$year)
  xlabels = function(x){month.abb[x]}
  
  ggplot2::ggplot(data = x,
                   mapping = ggplot2::aes(x = .data$month, 
                                          y = .data$MSL,
                                          color = .data$year,
                                          group = .data$year)) +
    ggplot2::geom_path() +
    ggplot2::scale_color_viridis_c(direction = 1, alpha = 0.7, option = "viridis") + 
    ggplot2::scale_x_continuous(breaks = 1:12,
                                labels = xlabels) + 
    ggplot2::geom_path(data = y,
                       mapping = ggplot2::aes(x = .data$month, y = .data$MSL),
                       col = "blue", linewidth = 2) +
    ggplot2::facet_wrap(~station)
  
}

#' @export
#' @rdname plot_tide_climatology
#' @param type chr one "timeseries" (default), "monthly" or "climatology"
plot_tide = function(x = read_tide(),
                     type = c("timeseries", "monthly", "climatology")[1]){
  
  if (tolower(type[1]) == "climatology") return(plot_tide_climatology(x))
  
  x = dplyr::group_by(x, .data$station)
  
  if (tolower(type[1]) == "timeseries"){
    gg = ggplot2::ggplot(data = x,
                    mapping = ggplot2::aes(x = .data$date, 
                                           y = .data$MSL)) +
      ggplot2::geom_path(color = "grey") +
      ggplot2::geom_ribbon(mapping = ggplot2::aes(x = .data$date, 
                                                  ymin = .data$Low_Conf,
                                                  ymax = .data$High_Conf),
                         fill = "blue", alpha = 0.8) +
      ggplot2::geom_line(mapping = ggplot2::aes(x = .data$date, 
                                                y = .data$Linear_Trend),
                         col = "white") + 
      ggplot2::facet_wrap(~station)
  } else {
    x = dplyr::mutate(x,
                      month = month.abb[.data$month] |>
                        factor(levels = month.abb))
    gg = ggplot2::ggplot(data = x,
                         mapping = ggplot2::aes(x = .data$year, 
                                                y = .data$MSL)) +
      ggplot2::geom_path(color = "grey") +
      ggplot2::geom_smooth(method = "lm",
                           formula = y ~ x,
                           se = FALSE,
                           col = "blue") +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x =ggplot2::element_blank()) + 
      ggplot2::facet_grid(station ~ month)
  }
    
  gg
}