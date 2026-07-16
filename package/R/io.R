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
