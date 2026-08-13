## Harvesting data from US Census Bureau API through package `tidycensus`

suppressPackageStartupMessages({
  library(tidycensus)
  
  library(dplyr)
  library(tidyr)
  library(readr)
})

me_counties = 


# county level

# ACS 1-year survey
# 2020 data is missing

acs1_vars = load_variables(2005, "acs1")

get_acs(geography = "county",
        state="ME",
        year = 2005,
        variable = c(population = "B01003_001",
                     housing = "B25001_001",
                     med_home_value = "B25077_001",
                     median_re_tax = "B25103_001"),
        survey="acs1") |>
  mutate(year = year, .after=GEOID) |>
  select(-moe)


# ACS 5-year survey
# starts in 2009, does not include med_re_tax

acs5_vars = load_variables(2009, "acs5")

get_acs(geography = "county",
        state="ME",
        year = 2009,
        variable = c(population = "B01003_001",
                     housing = "B25001_001",
                     med_home_value = "B25077_001"),
        survey="acs5") |>
  mutate(year = year, .after=GEOID) |>
  select(-moe)



census_maine_county = lapply(2009:2024,
                           function(year) {
                             get_acs(geography = "county",
                                     state="ME",
                                     year = year,
                                     variable = c(population = "B01003_001", 
                                                  housing = "B25001_001",
                                                  med_home_value = "B25077_001"),
                                     survey="acs5") |>
                               mutate(year = year, .after=GEOID)
                           }) |>
  bind_rows() |>
  arrange(GEOID) |>
  select(-moe) |> # do we want to keep margin of error?
  pivot_wider(names_from = variable, values_from = estimate) |> 
  filter(GEOID %in% me_counties$geoId,
         !grepl('County subdivisions not defined', NAME)) |>
  rename(geoid = GEOID,
         name = NAME)


write_csv(census_maine_county, "data/CENSUS/acs5_county_2009_2024.csv.gz")

# county subdivision level

# ACS 5-year survey is only option for sub county level data (starting in 2009)

get_acs(geography = "county subdivision",
        state="ME",
        year = 2009,
        variable = c(population = "B01003_001",
                     housing = "B25001_001",
                     med_home_value = "B25077_001"),
        survey="acs5") |>
  mutate(year = year, .after=GEOID) |>
  select(-moe)

census_maine_county_sub = lapply(2009:2024,
                           function(year) {
                             get_acs(geography = "county subdivision",
                                     state="ME",
                                     year = year,
                                     variable = c(population = "B01003_001", 
                                                  housing = "B25001_001",
                                                  med_home_value = "B25077_001"),
                                     survey="acs5") |>
                               mutate(year = year, .after=GEOID)
                           }) |>
  bind_rows() |>
  arrange(GEOID) |>
  select(-moe) |> # do we want to keep margin of error?
  pivot_wider(names_from = variable, values_from = estimate) |> 
  mutate(county = substr(GEOID, 1, 5), .after = GEOID) |>
  filter(county %in% me_counties$geoId,
         !grepl('County subdivisions not defined', NAME)) |>
  rename(geoid = GEOID,
         name = NAME)

write_csv(census_maine_county_sub, "data/CENSUS/acs5_county_subdivision_2009_2024.csv.gz")
