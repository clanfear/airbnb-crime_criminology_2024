library(tidyverse)
library(lubridate)
library(sf)
source("./syntax/project_functions.R")

load("./data/derived/crime/ward_crime_weighted.RData")

load("./data/derived/mopac/ce_ward_year_all_methods.RData")
load("./data/derived/shape/london_ward.RData")
load("./data/derived/airbnb/airdna_ward_yearly.RData")
load("./data/derived/airbnb/airdna_ward.RData")
load("./data/derived/property_prices/ward_year_property_prices_weighted.RData")


ward_year <- airdna_ward_yearly  |>
  inner_join(ward_crime_weighted |>
               # Important: Restrict the crime data to match the Airbnb data
               # If you don't do this, you get crime counts for all of 2018 
               # which will create negative association between Airbnb and crime
               filter(date >= ym("2015-01") & date <= ym("2018-03")) |>
               group_by(ward_code) |>
               arrange(date) |>
               # Linear interpolation up to four months in a row; this fills in only a tiny number of observations; most NAs are in blocks
               mutate(across(matches("^(dp|dlg)"), ~ zoo::na.approx(., maxgap = 4, rule = 2)), .groups = "drop") |>
               mutate(year = year(date)) |>
               group_by(ward_code, year) |>
               summarize(across(matches("^(dlg|dp)"), ~sum(., na.rm=TRUE)), .groups = "drop")) |> 
  # filter(date >= ymd("2015-04-01") & date < ymd("2018-04-01")) |>
  arrange(ward_code, year) |>
  left_join(ward_year_property_prices_weighted) %>%
  left_join(ce_ward_year_all_methods)

list_missing(ward_year)
nrow(ward_year) == n_distinct(ward_year$ward_code) * n_distinct(ward_year$year)

save(ward_year, file = "./data/analytical/ward_year.RData")

