# This script generates nearest neighbor index measures for determining if 
# Airbnb effects depend on how tightly clustered they are. This is not in the
# main paper but enters some sensitivity tests. Regardless, we keep the NNI
# measure in the main data. This is computationally intensive.

library(tidyverse)
library(lubridate)
library(sf)
library(furrr)
plan(multisession, workers = 8)

load("./data/derived/shape/london_lsoa.RData")
load("./data/derived/airbnb/airdna_monthly.RData")

# Calculate nearest neighbor distance
get_nnd <- function(geometry){
  if(length(geometry) == 1){
    return(NA)
  } else {
    . <- st_distance(geometry)
    diag(.) <- NA
    return(apply(., 2, \(x) min(x, na.rm=TRUE)))
  }
}

# Calculate average nearest neighbor distance
get_annd <- function(geometry){
  if(nrow(geometry) == 1){
    return(NA)
  } else {
    . <- st_distance(geometry)
    diag(.) <- NA
    return(mean(apply(., 2, \(x) min(x, na.rm=TRUE))))
  }
}

# LSOA

airdna_props_lsoa <-  airdna_monthly |>
  mutate(year_quarter = lubridate::quarter(date, type = "date_first")) |>
  distinct(property_id, year_quarter, geometry) |>
  st_join(london_lsoa) |>
  select(lsoa_code, year_quarter, geometry) |>
  group_by(lsoa_code, year_quarter) |>
  nest(data = geometry)

airdna_props_lsoa_annd <- airdna_props_lsoa
airdna_props_lsoa_annd$annd <- future_map_dbl(airdna_props_lsoa_annd$data, get_annd, .progress = TRUE, .options = furrr_options(seed = T))
airdna_props_lsoa_annd <- airdna_props_lsoa_annd %>% 
  select(-data) |> ungroup()

save(airdna_props_lsoa_annd, file = "./data/derived/airbnb/airdna_props_lsoa_annd.RData")

lsoa_airdna_erd <- london_lsoa |>
  full_join(airdna_props_lsoa %>% mutate(n_props = map_dbl(data, ~dim(.x)[1])) |> select(-data), by = "lsoa_code")|>
  mutate(area = st_area(geometry)) |>
  st_drop_geometry() |>
  mutate(erd = 0.5 / sqrt(n_props/as.numeric(area))) |> 
  select(lsoa_code, year_quarter, erd) |>
  arrange(lsoa_code, year_quarter)

save(lsoa_airdna_erd, file = "./data/derived/airbnb/lsoa_airdna_erd.RData")

lsoa_airdna_nni <- lsoa_airdna_erd |>
  full_join(airdna_props_lsoa_annd) |>
  mutate(nni = annd / erd) |>
  mutate(nni = ifelse(is.na(nni), 1, nni)) |>
  select(lsoa_code, year_quarter, nni)

save(lsoa_airdna_nni, file = "./data/derived/airbnb/lsoa_airdna_nni.RData")

