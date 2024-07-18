# This script runs the main models with two different covariates to address the
# possibility of spurious due to police activity: (1) stops / searches and (2)
# survey-reported police patrol frequency.

library(tidyverse)
library(sf)
library(dpm)
library(lme4)

load("./data/derived/shape/london_lsoa.RData")
load("./data/analytical/lsoa_quarter_props_crime.RData")
source("./syntax/project_functions.R")

# STOP AND SEARCH
# You can fetch the open access stop and search data from https://data.police.uk/data/
# I don't provide it in this repo due to size. Just sub in your file location.
ss_data <- list.files("C:/Users/cclan/Desktop/", recursive = TRUE, full.names = TRUE) |>
  str_subset("stop-and-search") |>
  str_subset("metropolitan|london") |> 
  read_csv() |>
  janitor::clean_names() |>
  filter(!is.na(longitude) & !is.na(latitude)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(st_crs(london_lsoa)) |>
  st_join(london_lsoa)

dpm_quarter_ss <- ss_data |>
  st_drop_geometry() |>
  mutate(year_quarter = lubridate::quarter(date, type = "date_first")) |>
  count(year_quarter, lsoa_code) |>
  complete(lsoa_code, year_quarter,
           fill = list(n = 0)) |>
  rename(stops = n) |>
  arrange(lsoa_code, year_quarter) |>
  inner_join(lsoa_quarter_props_crime) |>
  mutate(dlg_violence = dlg_violence_noharm + dlg_violence_harm) |>
  mutate(across(matches("^(dp|dlg|abnb|nni|rpp|stops)"), ~standardize(.))) %>%
  mutate(., stops_residual = residuals(lm(stops ~ dp_robbery + dp_burglary + dp_asb + dp_violence + dlg_theft + dlg_violence_harm, data = .))) |>
  mutate(date_num = as.numeric(year_quarter_fac)) |>
  panelr::panel_data(id = lsoa_code, wave = date_num)

list_missing(dpm_quarter_ss)


dpm_quarter_ss_fit <- expand.grid(dv = c("dp_robbery", "dp_burglary", "dp_asb", "dp_violence", "dlg_violence_harm", "dlg_theft"),
                                  spec = c("both", "con", "lag")) %>%
  mutate(
    form = case_when(
      spec == "both" ~ paste0(dv, " ~ ", "pre(abnb_active_rentals) + pre(lag(abnb_active_rentals)) + pre(stops) + pre(lag(stops))"),
      spec == "con"  ~ paste0(dv, " ~ ", "pre(abnb_active_rentals) + pre(stops)"),
      spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(abnb_active_rentals)) + pre(lag(stops))"),
      TRUE ~ "ERROR")) %>%
  run_dpm(dpm_quarter_ss)


dpm_coef_plot(dpm_quarter_ss_fit)

# Identical using residualized stops
dpm_quarter_ssr_fit <- expand.grid(dv = c("dp_robbery", "dp_burglary", "dp_asb", "dp_violence", "dlg_violence_harm", "dlg_theft"),
                                  spec = c("both", "con", "lag")) %>%
  mutate(
    form = case_when(
      spec == "both" ~ paste0(dv, " ~ ", "pre(abnb_active_rentals) + pre(lag(abnb_active_rentals)) + pre(stops_residual) + pre(lag(stops_residual))"),
      spec == "con"  ~ paste0(dv, " ~ ", "pre(abnb_active_rentals) + pre(stops_residual)"),
      spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(abnb_active_rentals)) + pre(lag(stops_residual))"),
      TRUE ~ "ERROR")) %>%
  run_dpm(dpm_quarter_ss)

dpm_coef_plot(dpm_quarter_ssr_fit)


# POLICE PATROLS
load("./data/derived/dpm_ward_half.RData")
load("./data/derived/mopac/pas_15_18_wide.RData")
lmer_patrols <- lmer(see_patrols ~ race + tenure_num + age_num + employment + homeowner + gender + victim_year  + (1|half_ward), REML = FALSE, data = pas_15_18_wide %>%
                       mutate(see_patrols = as.numeric(see_patrols),
                              half_ward = paste0(year_half, "_", ward_code)))

var_components <- insight::get_variance(lmer_patrols)
t00 <- var_components$var.intercept[[1]]
s2  <- var_components$var.residual
n_count <- table(insight::get_random(lmer_patrols))
J <- length(n_count) # number of neighbs
sum(t00 / (t00 + s2 / n_count)) / J

patrol_half_ward <- ranef(lmer_patrols, condVar = FALSE)$half_ward |> 
  rownames_to_column("half_ward") |> tibble() |> separate(half_ward, into = c("year_half", "ward_code"), sep = "_") |> 
  transmute(year_half = as.numeric(year_half), ward_code, patrols = standardize(`(Intercept)`)) |>
  arrange(ward_code, year_half) |>
  rename(year_half_num = year_half)

dpm_ward_half_patrols <- dpm_ward_half |>
  left_join(patrol_half_ward)

dpm_half_patrols_fit <- expand.grid(dv = c("log_std_dp_robbery", "log_std_dp_burglary", "log_std_dp_asb", "log_std_dp_violence", "log_std_dlg_violence_harm_aw", "log_std_dlg_theft_aw"),
                               spec = c("con", "lag", "both")) %>%
  mutate(
    form = case_when(
      spec == "both" ~ paste0(dv, " ~ ", "pre(std_abnb_active_rentals) + pre(lag(std_abnb_active_rentals)) + pre(patrols) + pre(lag(patrols))"),
      spec == "con"  ~ paste0(dv, " ~ ", "pre(std_abnb_active_rentals) + pre(patrols)"),
      spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(std_abnb_active_rentals)) + pre(lag(patrols))"),
      TRUE ~ "ERROR")) %>%
  run_dpm(., dpm_ward_half_patrols)

dpm_coef_plot(dpm_half_patrols_fit, hide_lag = TRUE)

