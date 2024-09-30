library(tidyverse)
library(dpm)
source("./syntax/project_functions.R")
load("./data/analytical/ward_year.RData")
list_missing(ward_year)

# Log then standardize; standardize across all data rather than within model
# as coefficients are fixed across waves; within-wave standardization will do
# strange things in that situation (same coef, different scales).

dpm_ward_year <- ward_year %>%
  mutate(across(matches("^(dp|dlg)"), ~log_na(.), .names = "log_{.col}")) %>%
  mutate(across(matches("^(rpp|ce|dp|dlg|abnb)"), ~standardize(.), .names = "std_{.col}")) %>%
  mutate(across(matches("^(rpp_me|dp|dlg|abnb)"), ~standardize(log_na(.)), .names = "log_std_{.col}")) %>%
  panelr::panel_data(id = ward_code, wave = year)

# Fit
dpm_year_fit <- expand.grid(dv = c("log_std_dp_robbery", "log_std_dp_burglary", "log_std_dp_asb", "log_std_dp_violence", "log_std_dlg_violence_harm_aw", "log_std_dlg_theft_aw"),
                               spec = c("both", "con", "lag")) %>%
  mutate(
    form = case_when(
    spec == "both" ~ paste0(dv, " ~ ", "pre(std_abnb_active_rentals) + pre(lag(std_abnb_active_rentals))"),
    spec == "con"  ~ paste0(dv, " ~ ", "pre(std_abnb_active_rentals) "),
    spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(std_abnb_active_rentals))"),
    TRUE ~ "ERROR")) %>%
  run_dpm(., dpm_ward_year)
save(dpm_year_fit, file = "./data/output/dpm_year_fit.RData")
dpm_coef_plot(dpm_year_fit, hide_lag = TRUE)


dpm_year_diffprops_fit <- expand.grid(dv = c("log_std_dp_robbery", "log_std_dp_burglary", "log_std_dp_asb", "log_std_dp_violence", "log_std_dlg_violence_harm_aw", "log_std_dlg_theft_aw"),
                            spec = c("both", "con", "lag")) %>%
  mutate(
    form = case_when(
      spec == "both" ~ paste0(dv, " ~ ", "pre(std_abnb_entire_home_apt) + pre(std_abnb_private_room) + pre(std_abnb_shared_room) +pre(lag(std_abnb_entire_home_apt)) + pre(lag(std_abnb_private_room)) + pre(lag(std_abnb_shared_room))"),
      spec == "con"  ~ paste0(dv, " ~ ", "pre(std_abnb_entire_home_apt) + pre(std_abnb_private_room) + pre(std_abnb_shared_room)"),
      spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(std_abnb_entire_home_apt)) + pre(lag(std_abnb_private_room)) + pre(lag(std_abnb_shared_room))"),
      TRUE ~ "ERROR")) %>%
  run_dpm(., dpm_ward_year)
dpm_coef_plot(dpm_year_diffprops_fit, hide_lag = TRUE)
save(dpm_year_diffprops_fit, file = "./data/output/dpm_year_diffprops_fit.RData")



dpm_year_ce_imputed_2lvl_fit <- expand.grid(dv = c("log_std_dp_robbery", "log_std_dp_burglary", "log_std_dp_asb", "log_std_dp_violence", "log_std_dlg_violence_harm_aw", "log_std_dlg_theft_aw"),
                                            spec = c("con", "lag", "both")) %>%
  mutate(
    form = case_when(
      spec == "both" ~ paste0(dv, " ~ ", "pre(std_abnb_active_rentals) + pre(lag(std_abnb_active_rentals)) + pre(std_ce_imputed_2lvl) + pre(lag(std_ce_imputed_2lvl))"),
      spec == "con"  ~ paste0(dv, " ~ ", "pre(std_abnb_active_rentals) + pre(std_ce_imputed_2lvl)"),
      spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(std_abnb_active_rentals)) + pre(lag(std_ce_imputed_2lvl))"),
      TRUE ~ "ERROR")) %>%
  run_dpm(., dpm_ward_year)

dpm_coef_plot(dpm_year_ce_imputed_2lvl_fit, hide_lag = TRUE)
save(dpm_year_ce_imputed_2lvl_fit, file = "./data/output/dpm_year_ce_imputed_2lvl_fit.RData")


dpm_year_ce_medianprop_fit <- expand.grid(dv = c("log_std_dp_robbery", "log_std_dp_burglary", "log_std_dp_asb", "log_std_dp_violence", "log_std_dlg_violence_harm_aw", "log_std_dlg_theft_aw"),
                                            spec = c("con", "lag", "both")) %>%
  mutate(
    form = case_when(
      spec == "both" ~ paste0(dv, " ~ ", "pre(std_abnb_active_rentals) + pre(lag(std_abnb_active_rentals)) + pre(std_ce_imputed_2lvl) + pre(lag(std_ce_imputed_2lvl)) + pre(log_std_rpp_median_price_aw) + pre(lag(log_std_rpp_median_price_aw))"),
      spec == "con"  ~ paste0(dv, " ~ ", "pre(std_abnb_active_rentals) + pre(std_ce_imputed_2lvl) + pre(log_std_rpp_median_price_aw)"),
      spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(std_abnb_active_rentals)) + pre(lag(std_ce_imputed_2lvl)) + pre(lag(log_std_rpp_median_price_aw))"),
      TRUE ~ "ERROR")) %>%
  run_dpm(., dpm_ward_year)

dpm_coef_plot(dpm_year_ce_medianprop_fit, hide_lag = TRUE)
save(dpm_year_ce_medianprop_fit, file = "./data/output/dpm_year_ce_medianprop_fit.RData")
# Lettings to CE

dpm_year_ce_abnb_fit <- bind_rows(
  expand.grid(dv = "std_ce_imputed_2lvl",
              spec = c("con", "lag", "both")) %>%
  mutate(
    form = case_when(
      spec == "both" ~ paste0(dv, " ~ ", "pre(std_abnb_active_rentals) + pre(lag(std_abnb_active_rentals))"),
      spec == "con"  ~ paste0(dv, " ~ ", "pre(std_abnb_active_rentals)"),
      spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(std_abnb_active_rentals))"),
      TRUE ~ "ERROR")) %>%
  run_dpm(., dpm_ward_year), 
  expand.grid(dv = "log_std_abnb_active_rentals",
              spec = c("con", "lag", "both")) %>%
  mutate(
    form = case_when(
      spec == "both" ~ paste0(dv, " ~ ", "pre(std_ce_imputed_2lvl) + pre(lag(std_ce_imputed_2lvl))"),
      spec == "con"  ~ paste0(dv, " ~ ", "pre(std_ce_imputed_2lvl)"),
      spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(std_ce_imputed_2lvl))"),
      TRUE ~ "ERROR")) %>%
  run_dpm(., dpm_ward_year, optim.method = "BFGS")
  ) %>%
  select(dv, spec, term, estimate, std.error, conf.low, conf.high) %>%
  mutate(term = ifelse(str_detect(term, "dp|dlg") & str_detect(term, "t - 1"), "crime (t - 1)", term),
         across(c(dv, term), ~str_replace(., "ce_imputed_2lvl", "Collective Efficacy")),
         across(c(dv, term), ~ str_to_title(str_replace_all(str_remove_all(., "(log_)|(std_)|(dlg_|dp_|abnb_)|(_aw$)?"), "_", " "))),
         spec = fct_rev(factor(str_to_title(spec), levels = c("Con", "Both", "Lag"))))

save(dpm_year_ce_abnb_fit, file = "./data/output/dpm_year_ce_abnb_fit.RData")

# Disaggregated lettings to CE

dpm_year_ce_diffprops_fit <- bind_rows(
  expand.grid(dv = "std_ce_imputed_2lvl",
              spec = c("con", "lag", "both")) %>%
    mutate(
      form = case_when(
        spec == "both" ~ paste0(dv, " ~ ", "pre(std_abnb_entire_home_apt) + pre(std_abnb_private_room) + pre(std_abnb_shared_room) +pre(lag(std_abnb_entire_home_apt)) + pre(lag(std_abnb_private_room)) + pre(lag(std_abnb_shared_room))"),
        spec == "con"  ~ paste0(dv, " ~ ", "pre(std_abnb_entire_home_apt) + pre(std_abnb_private_room) + pre(std_abnb_shared_room)"),
        spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(std_abnb_entire_home_apt)) + pre(lag(std_abnb_private_room)) + pre(lag(std_abnb_shared_room))"),
        TRUE ~ "ERROR")) %>%
    run_dpm(., dpm_ward_year)
) %>%
  select(dv, spec, term, estimate, std.error, conf.low, conf.high) %>%
  mutate(term = ifelse(str_detect(term, "dp|dlg") & str_detect(term, "t - 1"), "crime (t - 1)", term),
         across(c(dv, term), ~str_replace(., "ce_imputed_2lvl", "Collective Efficacy")),
         across(c(dv, term), ~ str_to_title(str_replace_all(str_remove_all(., "(log_)|(std_)|(dlg_|dp_|abnb_)|(_aw$)?"), "_", " "))),
         spec = fct_rev(factor(str_to_title(spec), levels = c("Con", "Both", "Lag"))))

save(dpm_year_ce_diffprops_fit, file = "./data/output/dpm_year_ce_diffprops_fit.RData")

# With property prices
dpm_year_ce_abnb_prop_fit <- bind_rows(
  expand.grid(dv = "std_ce_imputed_2lvl",
              spec = c("con", "lag", "both")) %>%
    mutate(
      form = case_when(
        spec == "both" ~ paste0(dv, " ~ ", "pre(std_abnb_active_rentals) + pre(lag(std_abnb_active_rentals))  + pre(log_std_rpp_median_price_aw) + pre(lag(log_std_rpp_median_price_aw))"),
        spec == "con"  ~ paste0(dv, " ~ ", "pre(std_abnb_active_rentals) + pre(log_std_rpp_median_price_aw)"),
        spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(std_abnb_active_rentals)) + pre(lag(log_std_rpp_median_price_aw))"),
        TRUE ~ "ERROR")) %>%
    run_dpm(., dpm_ward_year), 
  expand.grid(dv = "log_std_abnb_active_rentals",
              spec = c("con", "lag", "both")) %>%
    mutate(
      form = case_when(
        spec == "both" ~ paste0(dv, " ~ ", "pre(std_ce_imputed_2lvl) + pre(lag(std_ce_imputed_2lvl)) + pre(log_std_rpp_median_price_aw) + pre(lag(log_std_rpp_median_price_aw))"),
        spec == "con"  ~ paste0(dv, " ~ ", "pre(std_ce_imputed_2lvl) + pre(log_std_rpp_median_price_aw)"),
        spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(std_ce_imputed_2lvl)) + pre(lag(log_std_rpp_median_price_aw))"),
        TRUE ~ "ERROR")) %>%
    run_dpm(., dpm_ward_year, optim.method = "BFGS")
) %>%
  select(dv, spec, term, estimate, std.error, conf.low, conf.high) %>%
  mutate(term = ifelse(str_detect(term, "dp|dlg") & str_detect(term, "t - 1"), "crime (t - 1)", term),
         across(c(dv, term), ~str_replace(., "ce_imputed_2lvl", "Collective Efficacy")),
         across(c(dv, term), ~str_replace(., "rpp_median_price", "log(Med. Price)")),
         across(c(dv, term), ~ str_to_title(str_replace_all(str_remove_all(., "(log_)|(std_)|(dlg_|dp_|abnb_)|(_aw$)?"), "_", " "))),
         spec = fct_rev(factor(str_to_title(spec), levels = c("Con", "Both", "Lag"))))

save(dpm_year_ce_abnb_prop_fit, file = "./data/output/dpm_year_ce_abnb_prop_fit.RData")



# POIS
fixest::feols(dlg_theft_aw ~ std_abnb_active_rentals + std_ce_imputed_2lvl | ward_code + year, data = dpm_ward_year, panel.id = c("ward_code", "year"))
fixest::feols(dp_burglary ~ std_abnb_active_rentals + std_ce_imputed_2lvl | ward_code + year, data = dpm_ward_year, panel.id = c("ward_code", "year"))
fixest::fepois(dp_robbery ~ std_abnb_active_rentals + std_ce_imputed_2lvl | ward_code + year, data = dpm_ward_year, panel.id = c("ward_code", "year"))

fixest::feols(std_ce_imputed_2lvl ~ std_abnb_active_rentals + l(std_abnb_active_rentals)  | ward_code + year, data = dpm_ward_year, panel.id = c("ward_code", "year"))
fixest::feols(std_abnb_active_rentals ~ std_ce_imputed_2lvl + l(std_ce_imputed_2lvl)  | ward_code + year, data = dpm_ward_year, panel.id = c("ward_code", "year"))
fixest::fepois(abnb_active_rentals ~ std_ce_imputed_2lvl + l(std_ce_imputed_2lvl)  | ward_code + year, data = dpm_ward_year, panel.id = c("ward_code", "year"))
