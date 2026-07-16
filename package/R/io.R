#' Read a fixed-width format monthly file ala AMO and NAO
#' 
#' '
#' @description
#' `read_monthly_fwf` reads a fixed width text file with unlabeled
#' columns year, Jan, Feb, ..., Nov, Dec.  `read_amo` and `read_nao`
#' are wrappers around `read_monthly_fwf` that preferentially return the 
#' long format of data.
#' 
#' @export
#' @param filename chr path specification for the file
#' @param na chr, one or more flags indicating no data
#' @param form chr, either "wide" (default) or "long"
#' @return tibble
read_monthly_fwf = function(filename = oame_path("AMO", "amo.csv"),
                            na = c("", "NA", "NaN","-99.990"),
                            form = c("wide", "long")[1]){
  x = readr::read_fwf(filename,
                      col_types = rep("n", 13) |> paste(collapse = ""),
                      skip = 0,
                      na = na) |>
    rlang::set_names(c("year", month.abb))
  
  if (tolower(form[1]) == "long"){
    x = x |>
      tidyr::pivot_longer(dplyr::all_of(month.abb),
                          names_to = "month",
                          values_to = "value") |>
      dplyr::mutate(date = paste(.data$year, .data$month, "1") |>
                      as.Date(format = "%Y %b %d"),
                    .before = 1)
  }
  return(x)
}

#' Read NAO data
#' 
#' @rdname read_monthly_fwf
#' @export
#' @param filename chr the path specification for the file
read_nao = function(filename = oame_path("NAO", "nao.csv"),
                    form = c("wide", "long")[2]){
  
  read_monthly_fwf(filename, form = form)
}

#' Read AMO data
#' 
#' @rdname read_monthly_fwf
#' @export
#' @param filename chr the path specification for the file
read_amo = function(filename = oame_path("AMO", "amo.csv"),
                    form = c("wide", "long")[2]){
  
  read_monthly_fwf(filename, form = form)
}


#' Read ME DMR landings data
#' 
#' @description
#' [Maine's DMR landings data portal](https://mainedmr.shinyapps.io/Landings_Portal/) allows
#' for the download of "historic" (state wide annual totals per species) and "modern"
#' (per port per species annual totals) landings data.  Use this function to 
#' read historic, modern or merged data.
#'  
#' @export
#' @param when chr, one of "modern" (default), "historic" or "merged"
#' @return data frame
read_dmr_landings = function(when = c("modern", "historic", "merged")[3]){
  
  when = tolower(when[1])
  
  if (when[1] == "merged") {
    x = read_dmr_landings("modern")
    y = read_dmr_landings("historic") |>
      dplyr::mutate(port = "ME",
                    county = "ME",
                    lob_zone = "ME",
                    weight_type = NA_character_)
    r = dplyr::bind_rows(x,y)
  } else {
    pat = switch(when[1],
                 "modern" = "^.*_Modern_.*\\.csv$",
                 "historic" = "^.*_Historic_.*\\.csv$",
                 stop("when not known", when[1]))
    filename = list.files(oame_path("DMR", "landings"),
                          pattern = pat,
                          full.names = TRUE)
    r = readr::read_csv(filename, show_col_types = FALSE)
  }
  r
}