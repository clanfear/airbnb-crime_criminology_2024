library(tidyverse)
library(lubridate)
library(sf)
source("./syntax/project_functions.R")

load("./data/derived/crime/ward_crime_weighted.RData")
load("./data/derived/mopac/ce_ward_half_2lvl_imputed.RData")
load("./data/derived/airbnb/airdna_ward_half.RData")
load("./data/derived/property_prices/ward_half_property_prices_weighted.RData")

ward_half <- airdna_ward_half |>
  inner_join(ward_crime_weighted |>
               # We only go to 2018-03 here because that is max month for Airbnb data
               filter(date >= ym("2015-01") & date <= ym("2018-03")) |>
               group_by(ward_code) |>
               arrange(date) |>
               # Linear interpolation up to four months in a row; this fills in only a tiny number of observations; most NAs are in blocks
               mutate(across(matches("^(dp|dlg)"), ~ zoo::na.approx(., maxgap = 4, rule = 2)), .groups = "drop") |>
               mutate(year = year(date),
                      half = ifelse(month(date) >=7, 0.5, 0),
                      year_half = as.character(year + half)) |>
               group_by(ward_code, year_half) |>
               summarize(across(matches("^(dlg|dp)"), ~sum(., na.rm=TRUE)), .groups = "drop")) |> 
  left_join(ce_ward_half_2lvl_imputed) |>
  left_join(ward_half_property_prices_weighted |> mutate(year_half = as.character(year_half)))

list_missing(ward_half)
nrow(ward_half) == n_distinct(ward_half$ward_code) * n_distinct(ward_half$year_half)

save(ward_half, file = "./data/analytical/ward_half.RData")

# Finally, produce the analytical file in panelr format for DPM
# Log then standardize; standardize across all data rather than within model
# as coefficients are fixed across waves; within-wave standardization will do
# strange things in that situation (same coef, different scales).

dpm_ward_half <- ward_half %>%
  mutate(year_half_num = as.numeric(year_half),
         collective_efficacy = ce) |>
  mutate(across(matches("^(dp|dlg)"), ~log_na(.), .names = "log_{.col}")) %>%
  mutate(across(matches("^(rpp|ce|dp|dlg|abnb|collective)"), ~standardize(.), .names = "std_{.col}")) %>%
  mutate(across(matches("^(rpp_me|dp|dlg|abnb)"), ~standardize(log_na(.)), .names = "log_std_{.col}")) %>%
  panelr::panel_data(id = ward_code, wave = year_half_num)

save(dpm_ward_half, file = "./data/analytical/dpm_ward_half.RData")