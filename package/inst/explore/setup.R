# source this first when exploring data and code

suppressPackageStartupMessages({
  library(oame)
  library(dplyr)
  library(ggplot2)
  library(ggokabeito)
})

DMR = read_dmr_landings()
AMO = read_amo()
NAO = read_nao()
COUNTIES = read_me_counties()

