# A reviewer asked whether disorder might be a confounder, so we re-ran the
# models including perceived disorder from the PAS. It seems ambiguous whether
# it would be a confounder or a mediator, so we did not do this in any main text
# models. It has no effect on the results anyway.

library(tidyverse)
library(dpm)
source("./syntax/project_functions.R")
load("./data/analytical/ward_half.RData")
load("./data/derived/dis_ward_half_2lvl_imputed.RData")


dpm_ward_half <- ward_half %>%
  mutate(year_half_num = as.numeric(year_half),
         collective_efficacy = ce) |>
  left_join(dis_ward_half_2lvl_imputed) |>
  mutate(across(matches("^(dp|dlg)"), ~log_na(.), .names = "log_{.col}")) %>%
  mutate(across(matches("^(rpp|ce|dp|dlg|abnb|collective|dis)"), ~standardize(.), .names = "std_{.col}")) %>%
  mutate(across(matches("^(rpp_me|dp|dlg|abnb)"), ~standardize(log_na(.)), .names = "log_std_{.col}")) %>%
  panelr::panel_data(id = ward_code, wave = year_half_num)

dpm_half_dis_fit <- expand.grid(dv = c("log_std_dp_robbery", "log_std_dp_burglary", "log_std_dp_asb", "log_std_dp_violence", "log_std_dlg_violence_harm_aw", "log_std_dlg_theft_aw"),
                                spec = c("con", "lag", "both")) %>%
  mutate(
    form = case_when(
      spec == "both" ~ paste0(dv, " ~ ", "pre(std_abnb_active_rentals) + pre(lag(std_abnb_active_rentals)) + pre(std_dis) + pre(lag(std_dis))"),
      spec == "con"  ~ paste0(dv, " ~ ", "pre(std_abnb_active_rentals) + pre(std_dis)"),
      spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(std_abnb_active_rentals)) + pre(lag(std_dis))"),
      TRUE ~ "ERROR")) %>%
  run_dpm(., dpm_ward_half)

save(dpm_half_dis_fit, file = "./data/output/dpm_half_dis_fit.RData")
