#' Read NAO data
#' 
#' @export
#' @param filename chr the path specification for the file
#' @param form chr one of "long" or "wide" (default)
#' @return tibble
read_nao = function(filename = oame_path("NAO", "nao.csv")){
  
  cnames = c("year", month.abb)
  x = readLines(filename) |>
    stringr::str_squish() |>
    stringr::str_replace_all(stringr::fixed(" "), ",") |>
    paste(collapse = "\n") |>
    I() |>
    readr::read_csv(skip = 0,
                    col_names = c("year", month.abb),
                    col_types = rep("n", 13) |> paste(collapse = ""))
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