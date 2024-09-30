# This script runs all the ML-SEM models via dpm. Note I am using my own version
# of the package to correct a minor bug. Some of these can take a while to run.

library(tidyverse)
library(dpm)
source("./syntax/project_functions.R")

load("./data/analytical/lsoa_quarter.RData")

# Last minute preprocessing for the estimation
# Standardized LSOA quarter data
dpm_quarter <- lsoa_quarter %>%
  mutate(dlg_violence = dlg_violence_noharm + dlg_violence_harm) %>%
  mutate(across(matches("^(dp|dlg|abnb|nni|rpp)"), ~standardize(.))) %>%
  mutate(date_num = as.numeric(year_quarter_fac)) %>%
  panelr::panel_data(id = lsoa_code, wave = date_num)

# Unstandardized LSOA quarter data
dpm_quarter_unstd <- lsoa_quarter %>%
  mutate(dlg_violence = dlg_violence_noharm + dlg_violence_harm) %>%
  mutate(date_num = as.numeric(year_quarter_fac)) %>%
  panelr::panel_data(id = lsoa_code, wave = date_num)

# LSOA quarter models
dpm_quarter_fit <- expand.grid(dv = c("dp_robbery", "dp_burglary", "dp_asb", "dp_violence", "dlg_violence_harm", "dlg_theft"),
                               spec = c("both", "con", "lag")) %>%
  mutate(
    form = case_when(
    spec == "both" ~ paste0(dv, " ~ ", "pre(abnb_active_rentals) + pre(lag(abnb_active_rentals))"),
    spec == "con"  ~ paste0(dv, " ~ ", "pre(abnb_active_rentals)"),
    spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(abnb_active_rentals))"),
    TRUE ~ "ERROR")) %>%
  run_dpm(dpm_quarter)

dpm_coef_plot(dpm_quarter_fit)
save(dpm_quarter_fit, file = "./data/output/dpm_quarter_fit.RData")

# AIRBNB DIFFERENT PROPERTY TYPES
# This replaces the single active Airbnb count IV with separate IVs for each
# type of Airbnb property. This shows effect are mainly due to entire properties
# which is consistent with opportunity and social disorg.

dpm_quarter_diffprops_fit <- expand.grid(dv = c("dp_robbery", "dp_burglary", "dp_asb", "dp_violence", "dlg_violence_harm", "dlg_theft"),
                                         spec = c("both", "con", "lag")) %>%
  mutate(
    form = case_when(
      spec == "both" ~ paste0(dv, " ~ ", "pre(abnb_entire_home_apt) + pre(lag(abnb_entire_home_apt)) + pre(abnb_private_room) + pre(lag(abnb_private_room)) + pre(abnb_shared_room) + pre(lag(abnb_shared_room))"),
      spec == "con"  ~ paste0(dv, " ~ ", "pre(abnb_entire_home_apt) + pre(abnb_private_room) + pre(abnb_shared_room)"),
      spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(abnb_entire_home_apt)) + pre(lag(abnb_private_room)) + pre(lag(abnb_shared_room))"),
      TRUE ~ "ERROR")) %>%
  run_dpm(dpm_quarter)
dpm_coef_plot(dpm_quarter_diffprops_fit)
save(dpm_quarter_diffprops_fit, file = "./data/output/dpm_quarter_diffprops_fit.RData")


# Unstandardized

dpm_quarter_fit_unstd <- expand.grid(dv = c("dp_robbery", "dp_burglary", "dp_asb", "dp_violence", "dlg_violence_harm", "dlg_theft"),
                               spec = c("both", "con", "lag")) %>%
  mutate(
    form = case_when(
      spec == "both" ~ paste0(dv, " ~ ", "pre(abnb_active_rentals) + pre(lag(abnb_active_rentals))"),
      spec == "con"  ~ paste0(dv, " ~ ", "pre(abnb_active_rentals)"),
      spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(abnb_active_rentals))"),
      TRUE ~ "ERROR")) %>%
  run_dpm(dpm_quarter_unstd)

dpm_coef_plot(dpm_quarter_fit_unstd)
save(dpm_quarter_fit_unstd, file = "./data/output/dpm_quarter_fit_unstd.RData")


dpm_quarter_diffprops_fit_unstd <- expand.grid(dv = c("dp_robbery", "dp_burglary", "dp_asb", "dp_violence", "dlg_violence_harm", "dlg_theft"),
                                         spec = c("both", "con", "lag")) %>%
  mutate(
    form = case_when(
      spec == "both" ~ paste0(dv, " ~ ", "pre(abnb_entire_home_apt) + pre(lag(abnb_entire_home_apt)) + pre(abnb_private_room) + pre(lag(abnb_private_room)) + pre(abnb_shared_room) + pre(lag(abnb_shared_room))"),
      spec == "con"  ~ paste0(dv, " ~ ", "pre(abnb_entire_home_apt) + pre(abnb_private_room) + pre(abnb_shared_room)"),
      spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(abnb_entire_home_apt)) + pre(lag(abnb_private_room)) + pre(lag(abnb_shared_room))"),
      TRUE ~ "ERROR")) %>%
  run_dpm(dpm_quarter_unstd)
dpm_coef_plot(dpm_quarter_diffprops_fit_unstd)
save(dpm_quarter_diffprops_fit_unstd, file = "./data/output/dpm_quarter_diffprops_fit_unstd.RData")


