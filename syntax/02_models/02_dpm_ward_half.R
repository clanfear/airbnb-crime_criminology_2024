library(tidyverse)
library(dpm)
source("./syntax/project_functions.R")
load("./data/analytical/dpm_ward_half.RData")


dpm_half_fit <- expand.grid(dv = c("log_std_dp_robbery", "log_std_dp_burglary", "log_std_dp_asb", "log_std_dp_violence", "log_std_dlg_violence_harm_aw", "log_std_dlg_theft_aw"),
                            spec = c("both", "con", "lag")) %>%
  mutate(
    form = case_when(
      spec == "both" ~ paste0(dv, " ~ ", "pre(std_abnb_active_rentals) + pre(lag(std_abnb_active_rentals))"),
      spec == "con"  ~ paste0(dv, " ~ ", "pre(std_abnb_active_rentals) "),
      spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(std_abnb_active_rentals))"),
      TRUE ~ "ERROR")) %>%
  run_dpm(., dpm_ward_half)
dpm_coef_plot(dpm_half_fit, hide_lag = TRUE)

save(dpm_half_fit, file = "./data/output/dpm_half_fit.RData")

dpm_half_ce_fit <- expand.grid(dv = c("log_std_dp_robbery", "log_std_dp_burglary", "log_std_dp_asb", "log_std_dp_violence", "log_std_dlg_violence_harm_aw", "log_std_dlg_theft_aw"),
                               spec = c("con", "lag", "both")) %>%
  mutate(
    form = case_when(
      spec == "both" ~ paste0(dv, " ~ ", "pre(std_abnb_active_rentals) + pre(lag(std_abnb_active_rentals)) + pre(std_collective_efficacy) + pre(lag(std_collective_efficacy))"),
      spec == "con"  ~ paste0(dv, " ~ ", "pre(std_abnb_active_rentals) + pre(std_collective_efficacy)"),
      spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(std_abnb_active_rentals)) + pre(lag(std_collective_efficacy))"),
      TRUE ~ "ERROR")) %>%
  run_dpm(., dpm_ward_half)

dpm_coef_plot(dpm_half_ce_fit, hide_lag = TRUE)

save(dpm_half_ce_fit, file = "./data/output/dpm_half_ce_fit.RData")


dpm_half_diffprops_fit <- expand.grid(dv = c("log_std_dp_robbery", "log_std_dp_burglary", "log_std_dp_asb", "log_std_dp_violence", "log_std_dlg_violence_harm_aw", "log_std_dlg_theft_aw"),
                            spec = c("both", "con", "lag")) %>%
  mutate(
    form = case_when(
      spec == "both" ~ paste0(dv, " ~ ", "pre(std_abnb_entire_home_apt) + pre(std_abnb_private_room) + pre(std_abnb_shared_room) +pre(lag(std_abnb_entire_home_apt)) + pre(lag(std_abnb_private_room)) + pre(lag(std_abnb_shared_room))"),
      spec == "con"  ~ paste0(dv, " ~ ", "pre(std_abnb_entire_home_apt) + pre(std_abnb_private_room) + pre(std_abnb_shared_room)"),
      spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(std_abnb_entire_home_apt)) + pre(lag(std_abnb_private_room)) + pre(lag(std_abnb_shared_room))"),
      TRUE ~ "ERROR")) %>%
  run_dpm(., dpm_ward_half)
dpm_coef_plot(dpm_half_diffprops_fit, hide_lag = TRUE)
save(dpm_half_diffprops_fit, file = "./data/output/dpm_half_diffprops_fit.RData")


dpm_half_ce_medianprop_fit <- expand.grid(dv = c("log_std_dp_robbery", "log_std_dp_burglary", "log_std_dp_asb", "log_std_dp_violence", "log_std_dlg_violence_harm_aw", "log_std_dlg_theft_aw"),
                                            spec = c("con", "lag", "both")) %>%
  mutate(
    form = case_when(
      spec == "both" ~ paste0(dv, " ~ ", "pre(std_abnb_active_rentals) + pre(lag(std_abnb_active_rentals)) + pre(std_collective_efficacy) + pre(lag(std_collective_efficacy)) + pre(log_std_rpp_median_price_aw) + pre(lag(log_std_rpp_median_price_aw))"),
      spec == "con"  ~ paste0(dv, " ~ ", "pre(std_abnb_active_rentals) + pre(std_collective_efficacy) + pre(log_std_rpp_median_price_aw)"),
      spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(std_abnb_active_rentals)) + pre(lag(std_collective_efficacy)) + pre(lag(log_std_rpp_median_price_aw))"),
      TRUE ~ "ERROR")) %>%
  run_dpm(., dpm_ward_half)

dpm_coef_plot(dpm_half_ce_medianprop_fit, hide_lag = TRUE)
save(dpm_half_ce_medianprop_fit, file = "./data/output/dpm_half_ce_medianprop_fit.RData")
# Lettings to CE


dpm_half_ce_abnb_fit <- bind_rows(
  expand.grid(dv = "std_collective_efficacy",
              spec = c("con", "lag", "both")) %>%
  mutate(
    form = case_when(
      spec == "both" ~ paste0(dv, " ~ ", "pre(std_abnb_active_rentals) + pre(lag(std_abnb_active_rentals))"),
      spec == "con"  ~ paste0(dv, " ~ ", "pre(std_abnb_active_rentals)"),
      spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(std_abnb_active_rentals))"),
      TRUE ~ "ERROR")) %>%
  run_dpm(., dpm_ward_half), 
  expand.grid(dv = "log_std_abnb_active_rentals",
              spec = c("con", "lag", "both")) %>%
  mutate(
    form = case_when(
      spec == "both" ~ paste0(dv, " ~ ", "pre(std_collective_efficacy) + pre(lag(std_collective_efficacy))"),
      spec == "con"  ~ paste0(dv, " ~ ", "pre(std_collective_efficacy)"),
      spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(std_collective_efficacy))"),
      TRUE ~ "ERROR")) %>%
  run_dpm(., dpm_ward_half, optim.method = "BFGS")
  ) %>%
  select(dv, spec, term, estimate, std.error, conf.low, conf.high) %>%
  mutate(term = ifelse(str_detect(term, "dp|dlg") & str_detect(term, "t - 1"), "crime (t - 1)", term),
         across(c(dv, term), ~str_replace(., "std_collective_efficacy", "Collective Efficacy")),
         across(c(dv, term), ~ str_to_title(str_replace_all(str_remove_all(., "(log_)|(std_)|(dlg_|dp_|abnb_)|(_aw$)?"), "_", " "))),
         spec = fct_rev(factor(str_to_title(spec), levels = c("Con", "Both", "Lag"))))

save(dpm_half_ce_abnb_fit, file = "./data/output/dpm_half_ce_abnb_fit.RData")

# Disaggregated lettings to CE

dpm_half_ce_diffprops_fit <- bind_rows(
  expand.grid(dv = "std_collective_efficacy",
              spec = c("con", "lag", "both")) %>%
    mutate(
      form = case_when(
        spec == "both" ~ paste0(dv, " ~ ", "pre(std_abnb_entire_home_apt) + pre(std_abnb_private_room) + pre(std_abnb_shared_room) +pre(lag(std_abnb_entire_home_apt)) + pre(lag(std_abnb_private_room)) + pre(lag(std_abnb_shared_room))"),
        spec == "con"  ~ paste0(dv, " ~ ", "pre(std_abnb_entire_home_apt) + pre(std_abnb_private_room) + pre(std_abnb_shared_room)"),
        spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(std_abnb_entire_home_apt)) + pre(lag(std_abnb_private_room)) + pre(lag(std_abnb_shared_room))"),
        TRUE ~ "ERROR")) %>%
    run_dpm(., dpm_ward_half)
) %>%
  select(dv, spec, term, estimate, std.error, conf.low, conf.high) %>%
  mutate(term = ifelse(str_detect(term, "dp|dlg") & str_detect(term, "t - 1"), "crime (t - 1)", term),
         across(c(dv, term), ~str_replace(., "std_collective_efficacy", "Collective Efficacy")),
         across(c(dv, term), ~ str_to_title(str_replace_all(str_remove_all(., "(log_)|(std_)|(dlg_|dp_|abnb_)|(_aw$)?"), "_", " "))),
         spec = fct_rev(factor(str_to_title(spec), levels = c("Con", "Both", "Lag"))))

save(dpm_half_ce_diffprops_fit, file = "./data/output/dpm_half_ce_diffprops_fit.RData")

# With property prices
dpm_half_ce_abnb_prop_fit <- bind_rows(
  expand.grid(dv = "std_collective_efficacy",
              spec = c("con", "lag", "both")) %>%
    mutate(
      form = case_when(
        spec == "both" ~ paste0(dv, " ~ ", "pre(std_abnb_active_rentals) + pre(lag(std_abnb_active_rentals))  + pre(log_std_rpp_median_price_aw) + pre(lag(log_std_rpp_median_price_aw))"),
        spec == "con"  ~ paste0(dv, " ~ ", "pre(std_abnb_active_rentals) + pre(log_std_rpp_median_price_aw)"),
        spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(std_abnb_active_rentals)) + pre(lag(log_std_rpp_median_price_aw))"),
        TRUE ~ "ERROR")) %>%
    run_dpm(., dpm_ward_half), 
  expand.grid(dv = "log_std_abnb_active_rentals",
              spec = c("con", "lag", "both")) %>%
    mutate(
      form = case_when(
        spec == "both" ~ paste0(dv, " ~ ", "pre(std_collective_efficacy) + pre(lag(std_collective_efficacy)) + pre(log_std_rpp_median_price_aw) + pre(lag(log_std_rpp_median_price_aw))"),
        spec == "con"  ~ paste0(dv, " ~ ", "pre(std_collective_efficacy) + pre(log_std_rpp_median_price_aw)"),
        spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(std_collective_efficacy)) + pre(lag(log_std_rpp_median_price_aw))"),
        TRUE ~ "ERROR")) %>%
    run_dpm(., dpm_ward_half, optim.method = "BFGS")
) %>%
  select(dv, spec, term, estimate, std.error, conf.low, conf.high) %>%
  mutate(term = ifelse(str_detect(term, "dp|dlg") & str_detect(term, "t - 1"), "crime (t - 1)", term),
         across(c(dv, term), ~str_replace(., "std_collective_efficacy", "Collective Efficacy")),
         across(c(dv, term), ~str_replace(., "rpp_median_price", "log(Med. Price)")),
         across(c(dv, term), ~ str_to_title(str_replace_all(str_remove_all(., "(log_)|(std_)|(dlg_|dp_|abnb_)|(_aw$)?"), "_", " "))),
         spec = fct_rev(factor(str_to_title(spec), levels = c("Con", "Both", "Lag"))))

save(dpm_half_ce_abnb_prop_fit, file = "./data/output/dpm_half_ce_abnb_prop_fit.RData")

