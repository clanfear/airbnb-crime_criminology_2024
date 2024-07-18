library(tidyverse)
library(lubridate)
library(sf)
source("./syntax/project_functions.R")

load("./data/derived/crime/lsoa_crime_count.RData")
load("./data/derived/airbnb/lsoa_airdna_nni.RData")
load("./data/derived/neighbors/london_lsoa_neighbors.RData")
load("./data/derived/airbnb/airdna_lsoa_monthly.RData")
load("./data/derived/airbnb/airdna_lsoa_quarterly.RData")
load("./data/derived/property_prices/property_prices_lsoa_month.RData")
load("./data/derived/property_prices/property_prices_lsoa_quarter.RData")
load("./data/derived/london_lsoa_pop.RData")
load("./data/derived/shape/london_lsoa.RData")

# Monthly data
lsoa_month_props_crime <- airdna_lsoa_monthly |>
  left_join(lsoa_crime_count) |>
  left_join(property_prices_lsoa_month) |>
  left_join(london_lsoa_pop) |>
  mutate(year     = year(date),
         month    = month(date),
         date_fac = factor(date),
         date_num = as.numeric(date_fac)) |>
  group_by(lsoa_code) |>
  arrange(date) |>
  mutate(across(matches("^(dlg|dp)"), ~100000*(. / population), .names = "{.col}_rate")) |>
  ungroup() |>
  # Treatment dummies to test policies
  mutate(treat_1_perm = ifelse(date >= ymd("2015-3-26"), 1, 0),
         treat_2_perm = ifelse(date >= ymd("2016-4-01"), 1, 0),
         treat_1_immediate = ifelse(year == year(ymd("2015-4-1")) &
                                      month == month(ymd("2015-4-1")), 1, 0),
         treat_2_immediate = ifelse(year == year(ymd("2016-4-06")) &
                                      month == month(ymd("2016-4-01")), 1, 0))

lsoa_month_props_crime_splags <- london_lsoa_neighbors |>
  inner_join(lsoa_month_props_crime |> rename(neighbors = lsoa_code)) |>
  group_by(lsoa_code, date) |>
  summarize(across(matches("^(abnb|dlg|dp|rpp)"), ~ mean(., na.rm = TRUE), .names = "splag_{.col}"), .groups = "drop")

lsoa_month_props_crime <- lsoa_month_props_crime |> 
  left_join(lsoa_month_props_crime_splags)

list_missing(lsoa_month_props_crime)
nrow(lsoa_month_props_crime) == n_distinct(lsoa_month_props_crime$lsoa_code) * n_distinct(lsoa_month_props_crime$date)

save(lsoa_month_props_crime, file = "./data/analytical/lsoa_month_props_crime.RData")

# Quarterly data

lsoa_quarter_props_crime <- lsoa_month_props_crime |>
  filter(date >= ym("2015-01") & date < ym("2018-04")) |>
  mutate(year_quarter = lubridate::quarter(date, type = "date_first")) |>
  group_by(lsoa_code, year_quarter) |>
  summarize(across(matches("^(dlg|dp)"), ~sum(., na.rm=TRUE)),
            lsoa_area = first(lsoa_area),
            population = first(population),
            density = first(density),
            .groups = "drop") |>
  left_join(property_prices_lsoa_quarter |> rename(year_quarter = quarter)) |>
  left_join(airdna_lsoa_quarterly) |>
  left_join(lsoa_airdna_nni) |>
  mutate(nni = ifelse(abnb_active_rentals == 0, 1, nni),
         nni_rentals = nni*abnb_active_rentals) |>
  arrange(year_quarter) |>
  mutate(year     = year(year_quarter),
         year_quarter_fac = factor(year_quarter),
         year_quarter_num = as.numeric(year_quarter_fac),
         treat_1_perm = ifelse(year_quarter >= ymd("2015-3-26"), 1, 0),
         treat_2_perm = ifelse(year_quarter >= ymd("2016-4-01"), 1, 0),
         treat_1_immediate = ifelse(year(year_quarter) == year(ymd("2015-04-1")) &
                                      month(year_quarter) == month(ymd("2015-04-01")), 1, 0),
         treat_2_immediate = ifelse(year(year_quarter) == year(ymd("2016-04-01")) &
                                      month(year_quarter) == month(ymd("2016-04-01")), 1, 0)) |>
  left_join(london_lsoa |> st_drop_geometry() |> select(lsoa_code, city_of_london))

lsoa_quarter_props_crime_splags <- london_lsoa_neighbors |>
  inner_join(lsoa_quarter_props_crime |> rename(neighbors = lsoa_code)) |>
  group_by(lsoa_code, year_quarter) |>
  summarize(across(matches("^(abnb|dlg|dp|rpp)"), ~ mean(., na.rm = TRUE), .names = "splag_{.col}"), .groups = "drop")

lsoa_quarter_props_crime <- lsoa_quarter_props_crime |> 
  left_join(lsoa_quarter_props_crime_splags)

list_missing(lsoa_quarter_props_crime)
nrow(lsoa_quarter_props_crime) == n_distinct(lsoa_quarter_props_crime$lsoa_code) * n_distinct(lsoa_quarter_props_crime$year_quarter)

save(lsoa_quarter_props_crime, file = "./data/analytical/lsoa_quarter_props_crime.RData")

# These two are final processed data in panelr form for the DPM function
# Base LSOA quarter data
dpm_quarter <- lsoa_quarter_props_crime %>%
  mutate(dlg_violence = dlg_violence_noharm + dlg_violence_harm) %>%
  mutate(across(matches("^(dp|dlg|abnb|nni|rpp)"), ~standardize(.))) %>%
  mutate(date_num = as.numeric(year_quarter_fac)) %>%
  panelr::panel_data(id = lsoa_code, wave = date_num)

save(dpm_quarter, file = "./data/analytical/dpm_quarter.RData")

# Base LSOA quarter data with unstandardized vars
dpm_quarter_unstd <- lsoa_quarter_props_crime %>%
  mutate(dlg_violence = dlg_violence_noharm + dlg_violence_harm) %>%
  mutate(date_num = as.numeric(year_quarter_fac)) %>%
  panelr::panel_data(id = lsoa_code, wave = date_num)

save(dpm_quarter_unstd, file = "./data/analytical/dpm_quarter_unstd.RData")