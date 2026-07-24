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


#' Fetch NAO
#' 
#' @export
#' @param url chr, the URL for of the data source
#' @param path chr, the destination path
#' @return data frame of most recent data
fetch_nao = function(url = "https://www.cpc.ncep.noaa.gov/products/precip/CWlink/pna/norm.nao.monthly.b5001.current.ascii.table",
                     path = oame_path("NAO")){

   ok = download.file(url,
                      file.path(path, "NAO.csv"))     
   read_nao()
                     }
