library(tidyverse)
library(sf)
library(areal)
source("./syntax/project_functions.R")

load("./data/derived/shape/london_ward.RData")
load("./data/derived/shape/london_lsoa.RData")
load("./data/derived/shape/london_lsoa_2001.RData")
load("./data/derived/shape/london_water.RData")
load("./data/derived/crime/lsoa_crime_count.RData")
load("./data/derived/crime/dp_crime_lat_lon.RData")
load("./data/derived/property_prices/property_prices_lsoa_half.RData")
load("./data/derived/property_prices/property_prices_lsoa_year.RData")

st_erase <- function(x, y) {
  st_difference(x, st_make_valid(st_union(st_combine(y))))
}
# Focusing on using 2018 wards.
# Two tasks here: Match dp data with lat/lon to Wards, then areal weight dlg and dp data to wards.
# Can use the lat/lon DP data to compare to areal weighted to get a sense of accuracy
# Areal weight should be done removing water from both sets of shapes, could also use population?

# About 1 tenth of 1 percent of cases are outside city by lat/lon. They're spread all across country, but don't look erroneous because they're not in water anywhere.
# Bit under 1% either missing or not in same LSOA as police coded it; probably imprecision due to anonymization
dp_crime_ward <- dp_crime_lat_lon %>% 
  filter(!is.na(crime_type)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(st_crs(london_ward)) %>% 
  st_join(london_ward) %>%
  st_drop_geometry() %>%
  count(ward_code, date, crime_type) %>%
  complete(date = unique(dp_crime_lat_lon$date), ward_code = unique(london_ward$ward_code), crime_type = unique(dp_crime_lat_lon$crime_type), fill = list(n=0)) %>%
  select(ward_code, date, crime_type, n) %>%
  pivot_wider(names_from = crime_type, values_from = n)

# Chop water out of wards and LSOAs to make areal weighting more accurate

london_ward_nowater <- london_ward %>%
  st_erase(london_water) %>% 
  select(ward_code, geometry) 
london_lsoa_nowater <- london_lsoa %>% 
  st_erase(london_water) %>% 
  select(lsoa_code, geometry)

london_lsoa_2001_nowater <- london_lsoa_2001 %>% 
  st_erase(london_water) %>% 
  select(lsoa_code_2001, geometry)

# How to do the areal weight? Seems the thing to do is create weights and then join after.
# This has to be done twice because a small number of MOPAC surveys were done 
# apparently using 2001 LSOAs. Mysteries.

ward_lsoa_weights <- aw_intersect(london_ward_nowater, london_lsoa_nowater, "area") |>
  aw_total(london_lsoa_nowater, lsoa_code, "area", "total_area", weight = "total", type = "extensive") |>
  aw_weight("area", "total_area", "area_weight") %>% 
  arrange(ward_code, lsoa_code) %>%
  st_drop_geometry()

ward_lsoa_2001_weights <- aw_intersect(london_ward_nowater, london_lsoa_2001_nowater, "area") |>
  aw_total(london_lsoa_2001_nowater, lsoa_code_2001, "area", "total_area", weight = "total", type = "extensive") |>
  aw_weight("area", "total_area", "area_weight") %>% 
  arrange(ward_code, lsoa_code_2001) %>%
  st_drop_geometry()

# 90% of LSOAs are essentially entirely (99%) in one ward
# Roughly 95% of LSOAs have at least 90% of their area in one ward
# Only 9 LSOAs out of 4835 (0.19%) do not have at least 50% of their area in one ward

ward_lsoa_weights %>% group_by(lsoa_code) %>% arrange(desc(area_weight)) %>% slice(1L) %>% pull(area_weight) %>% quantile(c(0.001, 0.01, 0.05, 0.053, 0.1, 0.5, 0.9))

highest_ward_lsoa_weights <- ward_lsoa_weights %>% 
  group_by(lsoa_code) %>% 
  arrange(desc(area_weight)) %>%
  slice(1L) %>% 
  ungroup() %>% select(-area, -total_area, -area_weight)

highest_ward_lsoa_2001_weights <- ward_lsoa_2001_weights %>% 
  group_by(lsoa_code_2001) %>% 
  arrange(desc(area_weight)) %>%
  slice(1L) %>% 
  ungroup() %>% select(-area, -total_area, -area_weight)

ward_lsoa_index <- rbind(highest_ward_lsoa_weights,
      highest_ward_lsoa_2001_weights %>% 
        filter(!lsoa_code_2001 %in% highest_ward_lsoa_weights$lsoa_code) %>% 
        rename(lsoa_code = lsoa_code_2001))

save(ward_lsoa_index, file = "./data/derived/ward_lsoa_index.RData")

# Wow, okay, the areal weighting is really, really accurate.
# Everything is correlated > 0.99
ward_crime_weighted <- lsoa_crime_count %>% 
  mutate(dlg_burglary = ifelse(is.na(dlg_burglary), dp_burglary, dlg_burglary),
         dlg_robbery = ifelse(is.na(dlg_robbery), dp_robbery, dlg_robbery),
         across(c(dlg_theft, dlg_violence_harm, dlg_violence_noharm, dlg_weapon_possession), ~ifelse(is.na(.), 0, .))) %>%
  select(lsoa_code, date, matches("^dp_|^dlg_")) %>%
  inner_join(ward_lsoa_weights %>% select(lsoa_code, ward_code, area_weight)) %>%
  group_by(ward_code, date) %>%
  summarize(across(matches("^(dlg_|dp_)"), ~sum(.*area_weight)), 
            n_lsoas = n(),
            .groups = "drop") %>%
  rename_with( ~paste0(., "_aw"), .cols = matches("^(dlg_|dp_)")) %>%
  left_join(dp_crime_ward) %>%
  semi_join(london_ward %>% filter(!city_of_london))

ward_crime_weighted %>% select(matches("^(dlg|dp)")) %>% cor(use = "pairwise.complete")

save(ward_crime_weighted, file = "./data/derived/crime/ward_crime_weighted.RData")

# Property prices
# These will only be used in (half)yearly analyses, so do them separately using the weights and calc year values.

ward_lsoa_intensive_weights <- aw_intersect(london_ward_nowater, london_lsoa_nowater, "area") |>
  aw_total(london_lsoa_nowater, lsoa_code, "area", "total_area", type = "intensive", weight = "sum") |>
  aw_weight("area", "total_area", "area_weight") %>% 
  arrange(ward_code, lsoa_code) %>%
  st_drop_geometry()

ward_half_property_prices_weighted <- property_prices_lsoa_half %>%
  inner_join(ward_lsoa_intensive_weights %>% select(lsoa_code, ward_code, area_weight), relationship = "many-to-many") %>%
  group_by(ward_code, year_half) %>%
  summarize(across(c(rpp_mean_price, rpp_median_price, rpp_mean_area, rpp_median_area, rpp_mean_priceper, rpp_median_priceper), 
                   ~mean(.*area_weight, na.rm=TRUE)), 
            rpp_n_sales = sum(rpp_n_sales*area_weight, na.rm=TRUE),
            n_lsoas = n(),
            .groups = "drop") %>%
  rename_with( ~paste0(., "_aw"), .cols = matches("^rpp")) %>%
  semi_join(london_ward %>% filter(!city_of_london))
save(ward_half_property_prices_weighted, file = "./data/derived/property_prices/ward_half_property_prices_weighted.RData")

ward_year_property_prices_weighted <- property_prices_lsoa_year %>%
  inner_join(ward_lsoa_intensive_weights %>% select(lsoa_code, ward_code, area_weight), relationship = "many-to-many") %>%
  group_by(ward_code, year) %>%
  summarize(across(c(rpp_mean_price, rpp_median_price, rpp_mean_area, rpp_median_area, rpp_mean_priceper, rpp_median_priceper), 
                   ~mean(.*area_weight, na.rm=TRUE)), 
            rpp_n_sales = sum(rpp_n_sales*area_weight, na.rm=TRUE),
            n_lsoas = n(),
            .groups = "drop") %>%
  rename_with( ~paste0(., "_aw"), .cols = matches("^rpp")) %>%
  semi_join(london_ward %>% filter(!city_of_london))
save(ward_year_property_prices_weighted, file = "./data/derived/property_prices/ward_year_property_prices_weighted.RData")
