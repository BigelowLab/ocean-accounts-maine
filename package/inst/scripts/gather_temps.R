suppressPackageStartupMessages({
  library(andreas)
  library(oame)
  library(sf)
  library(stars)
  library(dplyr)
})

PATH = andreas::copernicus_path("chfc")
DB = andreas::read_database(PATH, multiple = TRUE)

murPATH <- mur_path("nwa")
murDB <- read_database(murPATH)