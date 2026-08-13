# test driving DMR landings data

dmr = read_dmr_landings("modern") |>
  dplyr::filter(!(.data$county %in% c("UK", "Not-Specified")))

dmr = read_me_counties() |>
  dplyr::left_join(dmr, by = "county")
