# A reviewer was concerned that our results could be sensitive to influential
# cases. We used a few different approaches to address this, including some
# extreme ones like just dropping the 10% of cases with the strongest Airbnb
# and crime correlation. Remarkably, everything holds.
library(tidyverse)
library(dpm)
source("./syntax/project_functions.R")
load("./data/analytical/dpm_quarter.RData")
load("./data/analytical/dpm_quarter_unstd.RData")

# Without influential cases

highest_r_lsoas <- dpm_quarter |>
  as_tibble() |>
  group_by(lsoa_code) |>
  filter(var(dp_robbery) > 0 & var(abnb_active_rentals) > 0) |>
  summarize(r = cor(dp_robbery, abnb_active_rentals)) |>
  arrange(desc(r)) |>
  pull(lsoa_code) |>
  head(451)

dpm_quarter_nohighr <- dpm_quarter |> filter(lsoa_code %!in% highest_r_lsoas)

dpm_quarter_nohighr_fit <- expand.grid(dv = c("dp_robbery", "dp_burglary", "dp_asb", "dp_violence", "dlg_violence_harm", "dlg_theft"),
                               spec = c("both", "con", "lag")) %>%
  mutate(
    form = case_when(
      spec == "both" ~ paste0(dv, " ~ ", "pre(abnb_active_rentals) + pre(lag(abnb_active_rentals))"),
      spec == "con"  ~ paste0(dv, " ~ ", "pre(abnb_active_rentals)"),
      spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(abnb_active_rentals))"),
      TRUE ~ "ERROR")) %>%
  run_dpm(dpm_quarter_nohighr)

dpm_coef_plot(dpm_quarter_nohighr_fit)

# Without low activity LSOAs

high_activity_lsoas <- dpm_quarter |>
  as_tibble() |>
  summarize(abnb_active_rentals = mean(abnb_active_rentals), .by = lsoa_code) |>
  filter(abnb_active_rentals > median(abnb_active_rentals)) |>
  pull(lsoa_code)

dpm_quarter_high_activity <- dpm_quarter |> filter(lsoa_code %in% high_activity_lsoas)

dpm_quarter_high_activity_fit <- expand.grid(dv = c("dp_robbery", "dp_burglary", "dp_asb", "dp_violence", "dlg_violence_harm", "dlg_theft"),
                                       spec = c("both", "con", "lag")) %>%
  mutate(
    form = case_when(
      spec == "both" ~ paste0(dv, " ~ ", "pre(abnb_active_rentals) + pre(lag(abnb_active_rentals))"),
      spec == "con"  ~ paste0(dv, " ~ ", "pre(abnb_active_rentals)"),
      spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(abnb_active_rentals))"),
      TRUE ~ "ERROR")) %>%
  run_dpm(dpm_quarter_high_activity)
dpm_coef_plot(dpm_quarter_high_activity_fit)

# Quadratic Form
# These are not identified due to high skew in the squared term

dpm_quarter_quad <- dpm_quarter |>
  ungroup() |>
  mutate(abnb_active_rentals_1 = standardize(poly(abnb_active_rentals, 2)[,1]),
         abnb_active_rentals_2 = standardize(poly(abnb_active_rentals, 2)[,2])) |>
  panelr::panel_data(id = lsoa_code, wave = date_num)

dpm_quarter_quad_fit <- expand.grid(dv = c("dp_robbery", "dp_burglary", "dp_asb", "dp_violence", "dlg_violence_harm", "dlg_theft"),
                               spec = c("con")) %>%
  mutate(
    form = case_when(
      # spec == "both" ~ paste0(dv, " ~ ", "pre(abnb_active_rentals_1) + pre(lag(abnb_active_rentals_1)) + pre(abnb_active_rentals_2) + pre(lag(abnb_active_rentals_2))"),
      spec == "con"  ~ paste0(dv, " ~ ", "pre(abnb_active_rentals_1) + pre(abnb_active_rentals_2)"),
      # spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(abnb_active_rentals_1)) + pre(lag(abnb_active_rentals_2))"),
      TRUE ~ "ERROR")) %>%
  run_dpm(dpm_quarter_quad)

print(dpm_quarter_quad_fit, n = 100)
dpm_coef_plot(dpm_quarter_fit, con_only = TRUE)

# Ward half-year

load("./data/analytical/ward_half.RData")
list_missing(ward_half)

# Log then standardize; standardize across all data rather than within model
# as coefficients are fixed across waves; within-wave standardization will do
# strange things in that situation (same coef, different scales).

dpm_ward_half_quad <- ward_half %>%
  mutate(year_half_num = as.numeric(year_half),
         collective_efficacy = ce) |>
  mutate(across(matches("^(dp|dlg)"), ~log_na(.), .names = "log_{.col}")) %>%
  mutate(across(matches("^(rpp|ce|dp|dlg|abnb|collective)"), ~standardize(.), .names = "std_{.col}")) %>%
  mutate(across(matches("^(rpp_me|dp|dlg|abnb)"), ~standardize(log_na(.)), .names = "log_std_{.col}")) %>%
  mutate(std_abnb_active_rentals_1 = standardize(poly(std_abnb_active_rentals, 2)[,1]),
         std_abnb_active_rentals_2 = standardize(poly(std_abnb_active_rentals, 2)[,2])) |>
  panelr::panel_data(id = ward_code, wave = year_half_num)


dpm_half_quad_fit <- expand.grid(dv = c("log_std_dp_robbery", "log_std_dp_burglary", "log_std_dp_asb", "log_std_dp_violence", "log_std_dlg_violence_harm_aw", "log_std_dlg_theft_aw"),
                            spec = c("con")) %>%
  mutate(
    form = case_when(
      spec == "both" ~ paste0(dv, " ~ ", "pre(std_abnb_active_rentals_1) + pre(lag(std_abnb_active_rentals_1))"),
      spec == "con"  ~ paste0(dv, " ~ ", "pre(std_abnb_active_rentals_1) + pre(std_abnb_active_rentals_2)"),
      spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(std_abnb_active_rentals_1))"),
      TRUE ~ "ERROR")) %>%
  run_dpm(., dpm_ward_half_quad, estimator = "MLR")
dpm_half_quad_fit
dpm_coef_plot(dpm_ward_half_quad_fit, hide_lag = TRUE)
