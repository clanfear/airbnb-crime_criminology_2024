# This script reruns the main models while removing the entirety of London's
# transit zone 1, i.e., the core of the city.
library(tidyverse)
library(sf)
library(dpm)
source("./syntax/project_functions.R")
load("./data/analytical/lsoa_quarter.RData")
load("./data/derived/shape/london_lsoa.RData")

# Constructed a Zone 1 shapefile by hand, can skip that and load the relevant data
# zone_1_stations <- st_read("C:/Users/charles.lanfear/Downloads/doc.kml")
# zone_1_hull <- st_union(zone_1_stations) |> 
#   st_convex_hull() |>
#   st_transform(st_crs(london_lsoa)) |>
#   st_as_sf() %>%
#   rename(geometry = x) %>%
#   mutate(in_zone_1 = TRUE)
# 
# london_lsoa_zone_1 <- london_lsoa %>%
#   st_join(zone_1_hull) %>%
#   mutate(in_zone_1 = ifelse(is.na(in_zone_1), FALSE, in_zone_1)) %>%
#   select(lsoa_code, in_zone_1) %>%
#   st_drop_geometry()

load("./data/raw/london_lsoa_zone_1.RData")

# NOT ZONE 1
dpm_quarter_not_zone_1 <- lsoa_quarter %>%
  inner_join(london_lsoa_zone_1) %>%
  filter(!in_zone_1) %>%
  mutate(dlg_violence = dlg_violence_noharm + dlg_violence_harm) %>%
  mutate(across(matches("^(dp|dlg|abnb|nni|rpp)"), ~standardize(.))) %>%
  mutate(date_num = as.numeric(year_quarter_fac)) %>%
  panelr::panel_data(id = lsoa_code, wave = date_num)

dpm_quarter_not_zone_1_fit <- expand.grid(dv = c("dp_robbery", "dp_burglary", "dp_asb", "dp_violence", "dlg_violence_harm", "dlg_theft"),
                               spec = c("both", "con", "lag")) %>%
  mutate(
    form = case_when(
      spec == "both" ~ paste0(dv, " ~ ", "pre(abnb_active_rentals) + pre(lag(abnb_active_rentals))"),
      spec == "con"  ~ paste0(dv, " ~ ", "pre(abnb_active_rentals)"),
      spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(abnb_active_rentals))"),
      TRUE ~ "ERROR")) %>%
  run_dpm(dpm_quarter_not_zone_1)

dpm_coef_plot(dpm_quarter_not_zone_1_fit)
save(dpm_quarter_not_zone_1_fit, file = "./data/output/dpm_quarter_not_zone_1_fit.RData")
