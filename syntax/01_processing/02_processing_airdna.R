# This script processes AirDNA data provided by the CDRC. These data cannot be
# publicly shared, so you will likely be unable to run this file.

library(tidyverse)
library(lubridate)
library(sf)

# Run scratch to rerun even computationally intensive things.
run_scratch <- FALSE

data_dir <- "Z:/CSI Projects/Airbnb/Data/CDRC/Data/"

load("./data/derived/shape/london_lsoa.RData")
load("./data/derived/shape/london_ward.RData")
load("./data/derived/london_lsoa_pop.RData")
source("./syntax/project_functions.R")

# Prop data need preprocessing to deal with extraneous commas and quotes
# This just reads in the csv file line by line and fixes the issues before
# allowing normal CSV processing functions to handle it.

if(file.exists("./data/derived/airbnb/airdna_monthly.RData") & !run_scratch){
  load("./data/derived/airbnb/airdna_monthly.RData")
} else{
  airdna_props <- readLines(paste0(data_dir, "safeguarded.airdna.Property_edited.csv")) |>
    str_replace_all(
      c("\",\"" = "midmidmid",
        "^\""   = "begbegbeg",
        "\"$"    = "endendend")
      ) |>
    str_remove_all("(\"|,)") |>
    str_replace_all(
      c("midmidmid" = "\",\"",
        "begbegbeg" = "\"",
        "endendend" = "\"")
    ) |>
    I() |>
    read_csv() |>
    janitor::clean_names() |>
    filter(city == "London") |>
    select(property_id, host_id, max_guests)
  
  airdna_monthly <- read_csv(paste0(data_dir, "safeguarded.airdna.Monthly.csv")) |>
    janitor::clean_names() %>%
    rename(date = reporting_month) |>
    filter(city == "London" & active == TRUE & !is.na(latitude) & !is.na(listing_type)) |>
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
    st_transform(27700) |>
    left_join(airdna_props) |>
    mutate(active_time           = (available_days + reservation_days) / lubridate::days_in_month(date),
           occupied_time         = reservation_days / lubridate::days_in_month(date),
           usage                 = occupied_time * max_guests, # Usage = occupancy rate * n beds
           usage_consv           = occupied_time + 0.5*(max_guests-1)*occupied_time, # Min one guest, assume 50% occupancy otherwise
           dedicated_prop        = as.numeric(listing_type == "Entire home/apt" & active_time >= 0.75),
           dedicated_usage       = usage * dedicated_prop, # Calc separately for dedicated props
           dedicated_usage_consv = usage_consv * dedicated_prop,
           entire_home_apt_usage = as.numeric(listing_type == "Entire home/apt") * usage,
           private_room_usage = as.numeric(listing_type == "Private room") * usage,
           shared_room_usage = as.numeric(listing_type == "Shared room") * usage) %>%
    group_by(date, host_id) %>%
    mutate(n_host_properties = n(),
           n_host_entire_homes = sum(listing_type == "Entire home/apt")) %>%
    ungroup()
  # Not using number of properties yet
  save(airdna_monthly, file = "./data/derived/airbnb/airdna_monthly.RData")
}

# Aggregating to LSOAs
if(file.exists("./data/derived/airbnb/airdna_lsoa_disagg.RData") & !run_scratch){
  load("./data/derived/airbnb/airdna_lsoa_disagg.RData")
} else{
  airdna_lsoa_disagg <- airdna_monthly |> 
    select(property_id, listing_type, active_time, occupied_time, usage, usage_consv, dedicated_prop, dedicated_usage, dedicated_usage_consv, entire_home_apt_usage, private_room_usage, shared_room_usage, date, geometry) |>
    rename_with(~ paste0("abnb_", .), .cols = -c(geometry, date)) |>
    st_join(london_lsoa |>
      select(lsoa_code, borough_code, geometry)) |>
    st_drop_geometry() |>
    filter(!is.na(lsoa_code)) 
  save(airdna_lsoa_disagg, file = "./data/derived/airbnb/airdna_lsoa_disagg.RData")
}

# MONTHLY
airdna_lsoa_usage_monthly <- airdna_lsoa_disagg |> 
  group_by(lsoa_code, date) |>
  summarize(across(c(abnb_active_time, 
                     abnb_occupied_time, 
                     abnb_usage_consv, 
                     abnb_usage, 
                     abnb_dedicated_prop,
                     abnb_dedicated_usage_consv, 
                     abnb_dedicated_usage, 
                     abnb_entire_home_apt_usage, 
                     abnb_private_room_usage, 
                     abnb_shared_room_usage), ~ sum(., na.rm=TRUE)), .groups = "drop") |>
  complete(lsoa_code = unique(london_lsoa$lsoa_code), date) |>
  mutate(across(matches("abnb_"), ~replace_na(., 0)))
  
list_missing(airdna_lsoa_usage_monthly)
nrow(airdna_lsoa_usage_monthly) == n_distinct(airdna_lsoa_disagg$date) * n_distinct(london_lsoa$lsoa_code)


airdna_lsoa_monthly <- airdna_lsoa_disagg |>
  count(lsoa_code, abnb_listing_type, date) |>
  complete(lsoa_code = unique(london_lsoa$lsoa_code), abnb_listing_type, date, fill = list(n=0)) |>
  pivot_wider(names_from = abnb_listing_type, values_from = n) |>
  janitor::clean_names() |>
  rename_with(~ paste0("abnb_", .), .cols = c(entire_home_apt, private_room, shared_room)) |>
  left_join(airdna_lsoa_usage_monthly) |>
  mutate(abnb_active_rentals = abnb_entire_home_apt + abnb_private_room + abnb_shared_room)

list_missing(airdna_lsoa_monthly)
nrow(airdna_lsoa_monthly) ==  n_distinct(airdna_lsoa_disagg$date) * n_distinct(london_lsoa$lsoa_code)

save(airdna_lsoa_monthly, file = "./data/derived/airbnb/airdna_lsoa_monthly.RData")

# Density data for Figure 1 map of London
abnb_density_data <- london_lsoa %>%
  inner_join(
    airdna_lsoa_monthly %>%
      group_by(lsoa_code) %>%
      summarize(abnb_active_rentals = mean(abnb_active_rentals),
                abnb_dedicated_prop = mean(abnb_dedicated_prop))) %>%
  inner_join(london_lsoa_pop) %>%
  mutate(abnb_density    = as.numeric(abnb_active_rentals / lsoa_area),
         popden = density*100)

save(abnb_density_data, file = "./data/output/abnb_density_data.RData")

# QUARTERLY
airdna_lsoa_usage_quarterly <- airdna_lsoa_disagg |> 
  mutate(year_quarter = lubridate::quarter(date, type = "date_first")) |>
  group_by(lsoa_code, year_quarter) |>
  summarize(across(c(abnb_active_time, 
                     abnb_occupied_time, 
                     abnb_usage_consv, 
                     abnb_usage, 
                     abnb_dedicated_usage_consv, 
                     abnb_dedicated_usage, 
                     abnb_entire_home_apt_usage, 
                     abnb_private_room_usage, 
                     abnb_shared_room_usage), ~ sum(., na.rm=TRUE)), .groups = "drop")  |>
  complete(lsoa_code = unique(london_lsoa$lsoa_code), year_quarter) |>
  mutate(across(matches("abnb_"), ~replace_na(., 0)))

list_missing(airdna_lsoa_usage_quarterly)
nrow(airdna_lsoa_usage_quarterly) == n_distinct(airdna_lsoa_usage_quarterly$year_quarter) * n_distinct(airdna_lsoa_usage_quarterly$lsoa_code)

airdna_lsoa_quarterly <- airdna_lsoa_disagg |>
  mutate(year_quarter = lubridate::quarter(date, type = "date_first")) %>%
  distinct(abnb_property_id, lsoa_code, abnb_listing_type, year_quarter) |>
  count(lsoa_code, abnb_listing_type, year_quarter) |>
  complete(lsoa_code = unique(london_lsoa$lsoa_code), abnb_listing_type, year_quarter, fill = list(n=0)) |>
  pivot_wider(names_from = abnb_listing_type, values_from = n) |>
  janitor::clean_names() |>
  rename_with(~ paste0("abnb_", .), .cols = c(entire_home_apt, private_room, shared_room)) |>
  left_join(airdna_lsoa_disagg |>
              mutate(year_quarter = lubridate::quarter(date, type = "date_first")) |>
              filter(abnb_dedicated_prop == 1) |>
              distinct(lsoa_code, borough_code, abnb_property_id, year_quarter) |>
              count(lsoa_code, borough_code, year_quarter) |>
              rename(abnb_dedicated_prop = n)) |>
  mutate(abnb_dedicated_prop = replace_na(abnb_dedicated_prop, 0)) |>
  left_join(airdna_lsoa_usage_quarterly) |>
  mutate(abnb_active_rentals = abnb_entire_home_apt + abnb_private_room + abnb_shared_room)

list_missing(airdna_lsoa_quarterly)
nrow(airdna_lsoa_quarterly) ==  n_distinct(airdna_lsoa_quarterly$year_quarter) * n_distinct(airdna_lsoa_quarterly$lsoa_code)

save(airdna_lsoa_quarterly, file = "./data/derived/airbnb/airdna_lsoa_quarterly.RData")

###
# WARDS
###

if(file.exists("./data/derived/airbnb/airdna_ward_disagg.RData") & !run_scratch){
  load("./data/derived/airbnb/airdna_ward_disagg.RData")
} else{
  airdna_ward_disagg <- airdna_monthly |> 
    select(property_id, listing_type, active_time, occupied_time, usage, usage_consv, dedicated_prop, dedicated_usage, dedicated_usage_consv, entire_home_apt_usage, private_room_usage, shared_room_usage, date, geometry) |>
    rename_with(~ paste0("abnb_", .), .cols = -c(geometry, date)) |>
    st_join(london_ward |>
              select(ward_code, geometry)) |>
    st_drop_geometry() |>
    filter(!is.na(ward_code)) |>
    # Only have two months in 2014 so exclude, run up through end of first quarter 2018
    filter(date >= ym("2015-01") & date <= ym("2018-03")) |>
    mutate(year = lubridate::year(date),
           half = ifelse(month(date) >=7, 0.5, 0),
           year_half = as.character(year + half)) 
  save(airdna_ward_disagg, file = "./data/derived/airbnb/airdna_ward_disagg.RData")
}

# WARD HALF-YEAR

airdna_ward_usage_half <- airdna_ward_disagg |>
  group_by(ward_code, year_half) |>
  summarize(across(c(abnb_active_time, 
                     abnb_occupied_time, 
                     abnb_usage_consv, 
                     abnb_usage, 
                     abnb_dedicated_usage_consv, 
                     abnb_dedicated_usage, 
                     abnb_entire_home_apt_usage, 
                     abnb_private_room_usage, 
                     abnb_shared_room_usage), ~ sum(., na.rm=TRUE)), .groups = "drop")  |>
  complete(ward_code = unique(london_ward$ward_code), year_half) |>
  mutate(across(matches("abnb_"), ~replace_na(., 0)))

list_missing(airdna_ward_usage_half)
nrow(airdna_ward_usage_half) ==  n_distinct(airdna_ward_usage_half$year_half) * n_distinct(airdna_ward_usage_half$ward_code)

airdna_ward_half <- airdna_ward_disagg |>
  count(ward_code, abnb_listing_type, year_half) |>
  complete(ward_code = unique(london_ward$ward_code), abnb_listing_type, year_half, fill = list(n=0)) |>
  pivot_wider(names_from = abnb_listing_type, values_from = n) |>
  janitor::clean_names() |>
  rename_with(~ paste0("abnb_", .), .cols = c(entire_home_apt, private_room, shared_room)) |>
  left_join(airdna_ward_disagg |>
              filter(abnb_dedicated_prop == 1) |>
              count(ward_code, year_half) |>
              rename(abnb_dedicated_prop = n)) |>
  mutate(abnb_dedicated_prop = replace_na(abnb_dedicated_prop, 0)) |>
  left_join(airdna_ward_usage_half) |>
  mutate(abnb_active_rentals = abnb_entire_home_apt + abnb_private_room + abnb_shared_room)

list_missing(airdna_ward_half)
nrow(airdna_ward_half) ==  n_distinct(airdna_ward_half$year_half) * n_distinct(airdna_ward_half$ward_code)

save(airdna_ward_half, file = "./data/derived/airbnb/airdna_ward_half.RData")
# YEARS

airdna_ward_usage_yearly <- airdna_ward_disagg |> 
  group_by(ward_code, year) |>
  summarize(across(c(abnb_active_time, 
                     abnb_occupied_time, 
                     abnb_usage_consv, 
                     abnb_usage, 
                     abnb_dedicated_usage_consv, 
                     abnb_dedicated_usage, 
                     abnb_entire_home_apt_usage, 
                     abnb_private_room_usage, 
                     abnb_shared_room_usage), ~ sum(., na.rm=TRUE)), .groups = "drop")  |>
  complete(ward_code = unique(london_ward$ward_code), year) |>
  mutate(across(matches("abnb_"), ~replace_na(., 0)))

list_missing(airdna_ward_usage_yearly)
nrow(airdna_ward_usage_yearly) ==  n_distinct(airdna_ward_usage_yearly$year) * n_distinct(airdna_ward_usage_yearly$ward_code)

airdna_ward_yearly <- airdna_ward_disagg |>
  count(ward_code, abnb_listing_type, year) |>
  complete(ward_code = unique(london_ward$ward_code), abnb_listing_type, year, fill = list(n=0)) |>
  pivot_wider(names_from = abnb_listing_type, values_from = n) |>
  janitor::clean_names() |>
  rename_with(~ paste0("abnb_", .), .cols = c(entire_home_apt, private_room, shared_room)) |>
  left_join(airdna_ward_disagg |>
              filter(abnb_dedicated_prop == 1) |>
              count(ward_code, year) |>
              rename(abnb_dedicated_prop = n)) |>
  mutate(abnb_dedicated_prop = replace_na(abnb_dedicated_prop, 0)) |>
  left_join(airdna_ward_usage_yearly) |>
  mutate(abnb_active_rentals = abnb_entire_home_apt + abnb_private_room + abnb_shared_room)

list_missing(airdna_ward_yearly)
nrow(airdna_ward_yearly) ==  n_distinct(airdna_ward_yearly$year) * n_distinct(airdna_ward_yearly$ward_code)

save(airdna_ward_yearly, file = "./data/derived/airbnb/airdna_ward_yearly.RData")
