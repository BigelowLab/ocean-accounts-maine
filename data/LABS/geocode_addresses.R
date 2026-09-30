# Geocode addresses of institutions

library(readr)
library(tidygeocoder)

foo = read_csv("Maine Marine Science Institutions Curated.csv")

x <- foo |>
  geocode(address, method = 'osm', lat = latitude , long = longitude)

x <- foo |>
  geocode(address, method = 'census', lat = latitude , long = longitude)


cascade_results1 <- foo |>
  geocode_combine(
    queries = list(
      list(method = 'census'),
      list(method = 'osm')
    ),
    global_params = list(address = 'address')
  )




# separate geocoding for only new ones
x

write_csv(cascade_results1, "me_marine_labs.csv.gz")
