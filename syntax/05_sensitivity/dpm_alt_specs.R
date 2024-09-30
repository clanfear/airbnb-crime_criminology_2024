# DO NOT RUN THIS ALL AT ONCE, IT WILL GO FOR HOURS
# This script compiles many different alternate specifications noted in the 
# paper's text or asked for by reviewers.

library(tidyverse)
library(dpm)
library(ggtext)
library(furrr)
future::plan(multisession, workers = 6)

source("./syntax/project_functions.R")

load("./data/analytical/lsoa_quarter.RData")

# Last minute preprocessing for the estimation
# Standardized LSOA quarter data
dpm_quarter <- lsoa_quarter %>%
  mutate(dlg_violence = dlg_violence_noharm + dlg_violence_harm) %>%
  mutate(across(matches("^(dp|dlg|abnb|nni|rpp)"), ~standardize(.))) %>%
  mutate(date_num = as.numeric(year_quarter_fac)) %>%
  panelr::panel_data(id = lsoa_code, wave = date_num)

dpm_plot <- function(x){
  dv_levels <- c("Robbery", "Burglary", "Theft", "ASB", "Violence", "Harm")
  plot_data <- x %>%
    select(dv, spec, term, estimate, std.error, conf.low, conf.high) %>%
    mutate(term = ifelse(str_detect(term, "dp|dlg") & str_detect(term, "t - 1"), "dv (t - 1)", term),
           across(c(dv, term), ~ str_to_title(str_replace_all(str_remove(., "dlg_|dp_|abnb_"), "_", " "))),
           spec = fct_rev(str_to_title(spec)),
           dv = fct_relevel(str_replace_all(dv, c("Violence Harm" = "Harm", "Asb" = "ASB")), dv_levels)) %>%
    filter(term != "Dv (T - 1)")
  ggplot(plot_data, aes(x = estimate, y = spec, group = dv)) + 
    geom_rect(data = filter(plot_data, dv %in% dv_levels[c(2,4,6)] & spec == "Both"), fill = "black", xmin = -Inf,xmax = Inf,
              ymin = -Inf,ymax = Inf, alpha = 0.1) +
    ylab("**Crime type** and *Specification*") + xlab(NULL) +
    geom_point() + 
    facet_grid(dv ~ term, switch = "y") + 
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), size = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed") + 
    theme_minimal() + 
    labs(title = "ML-SEM Estimated Effects on Crime", caption = "Fully standardized, 95% confidence intervals") +
    theme(panel.grid.major.y  = element_blank(),
          panel.grid.minor.x  = element_blank(),
          panel.spacing.y =unit(0,"lines"),
          strip.placement = "outside",
          strip.text.y = element_text(face = "bold"),
          axis.text.y = element_text(face = "italic"),
          axis.title.y = element_markdown())
}

# LSOA quarter models with DLG for comparison
dpm_quarter_fit_dlg <- expand.grid(dv = c("dlg_robbery", "dlg_burglary", "dp_asb", "dlg_violence", "dlg_violence_harm", "dlg_theft"),
                                   spec = c("both", "con", "lag")) %>%
  mutate(
    form = case_when(
      spec == "both" ~ paste0(dv, " ~ ", "pre(abnb_active_rentals) + pre(lag(abnb_active_rentals))"),
      spec == "con"  ~ paste0(dv, " ~ ", "pre(abnb_active_rentals)"),
      spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(abnb_active_rentals))"),
      TRUE ~ "ERROR")) %>%
  run_dpm(dpm_quarter)

dpm_coef_plot(dpm_quarter_fit_dlg)

# Different property types but with usage measure
dpm_quarter_diffprops_usage_fit <- expand.grid(dv = c("dp_robbery", "dp_burglary", "dp_asb", "dp_violence", "dlg_violence_harm", "dlg_theft"),
                                         spec = c("both", "con", "lag")) %>%
  mutate(
    form = case_when(
      spec == "both" ~ paste0(dv, " ~ ", "pre(abnb_entire_home_apt_usage) + pre(lag(abnb_entire_home_apt_usage)) + pre(abnb_private_room_usage) + pre(lag(abnb_private_room_usage)) + pre(abnb_shared_room_usage) + pre(lag(abnb_shared_room_usage))"),
      spec == "con"  ~ paste0(dv, " ~ ", "pre(abnb_entire_home_apt_usage) + pre(abnb_private_room_usage) + pre(abnb_shared_room_usage)"),
      spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(abnb_entire_home_apt_usage)) + pre(lag(abnb_private_room_usage)) + pre(lag(abnb_shared_room_usage))"),
      TRUE ~ "ERROR")) %>%
  run_dpm(dpm_quarter)

save(dpm_quarter_diffprops_usage_fit, file = "./data/output/dpm_quarter_diffprops_usage_fit.RData")

dpm_quarter_diffprops_usage_fit %>% print(n=100)
dpm_plot(dpm_quarter_diffprops_usage_fit)

# No city of london

dpm_quarter_nocol <- lsoa_quarter %>%
  filter(!city_of_london) %>%
  mutate(across(matches("^(dp|dlg|abnb|nni|rpp)"), ~standardize(.))) %>%
  mutate(date_num = as.numeric(year_quarter_fac)) %>%
  panelr::panel_data(id = lsoa_code, wave = date_num)

dpm_quarter_fit_nocol <- expand.grid(dv = c("dp_robbery", "dp_burglary", "dp_asb", "dp_violence", "dlg_violence_harm", "dlg_theft"),
                                     spec = c("both", "con", "lag")) %>%
  mutate(
    form = case_when(
      spec == "both" ~ paste0(dv, " ~ ", "pre(abnb_active_rentals) + pre(lag(abnb_active_rentals))"),
      spec == "con"  ~ paste0(dv, " ~ ", "pre(abnb_active_rentals)"),
      spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(abnb_active_rentals))"),
      TRUE ~ "ERROR")) %>%
  run_dpm(dpm_quarter_nocol)

dpm_coef_plot(dpm_quarter_fit_nocol)

# AIRBNB DENSITY
# Replace count of Airbnb's with airbnb's per square kilometer. Just an alternate
# functional form assuming multiplicative rather than additive effect of LSOA size.

dpm_quarter_density_fit <- expand.grid(dv = c("dp_robbery", "dp_burglary", "dp_asb", "dp_violence", "dlg_violence_harm", "dlg_theft"),
                               spec = c("both", "con", "lag")) %>%
  mutate(
    form = case_when(
      spec == "both" ~ paste0(dv, " ~ ", "pre(abnb_density) + pre(lag(abnb_density))"),
      spec == "con"  ~ paste0(dv, " ~ ", "pre(abnb_density)"),
      spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(abnb_density))"),
      TRUE ~ "ERROR")) %>%
  run_dpm(dpm_quarter)

save(dpm_quarter_density_fit, file = "./data/output/dpm_alt_specs/dpm_quarter_density_fit.RData")

# AIRBNB DEDICATED
# Replace count of all Airbnb's with count of dedicated Airbnb properties, which
# are defined as entire home/apartments with > 75% active time. Interestingly,
# this shows slightly weaker relationship than al properties together. SOcial disorg
# would expect stronger effect.

dpm_quarter_dedicated_fit <- expand.grid(dv = c("dp_robbery", "dp_burglary", "dp_asb", "dp_violence", "dlg_violence_harm", "dlg_theft"),
                                         spec = c("both", "con", "lag")) %>%
  mutate(
    form = case_when(
      spec == "both" ~ paste0(dv, " ~ ", "pre(abnb_dedicated_prop) + pre(lag(abnb_dedicated_prop))"),
      spec == "con"  ~ paste0(dv, " ~ ", "pre(abnb_dedicated_prop)"),
      spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(abnb_dedicated_prop))"),
      TRUE ~ "ERROR")) %>%
  run_dpm(dpm_quarter)

save(dpm_quarter_dedicated_fit, file = "./data/output/dpm_alt_specs/dpm_quarter_dedicated_fit.RData")

# AIRBNB USAGE
# This substitutes an estimate of occupancy in as the Airbnb IV. Opportunity 
# explanations would expect usage to be a better predictor than raw properties.

dpm_quarter_usage_fit <- expand.grid(dv = c("dp_robbery", "dp_burglary", "dp_asb", "dp_violence", "dlg_violence_harm", "dlg_theft"),
                                     spec = c("both", "con", "lag")) %>%
  mutate(
    form = case_when(
      spec == "both" ~ paste0(dv, " ~ ", "pre(abnb_usage) + pre(lag(abnb_usage))"),
      spec == "con"  ~ paste0(dv, " ~ ", "pre(abnb_usage)"),
      spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(abnb_usage))"),
      TRUE ~ "ERROR")) %>%
  run_dpm(dpm_quarter)

save(dpm_quarter_usage_fit, file = "./data/output/dpm_alt_specs/dpm_quarter_usage_fit.RData")

# AIRBNB DEDICATED USAGE
# This substitutes an estimate of occupancy in as the Airbnb IV. Opportunity 
# explanations would expect usage to be a better predictor than raw properties.

dpm_quarter_dedicated_usage_fit <- expand.grid(dv = c("dp_robbery", "dp_burglary", "dp_asb", "dp_violence", "dlg_violence_harm", "dlg_theft"),
                                     spec = c("both", "con", "lag")) %>%
  mutate(
    form = case_when(
      spec == "both" ~ paste0(dv, " ~ ", "pre(abnb_dedicated_usage) + pre(lag(abnb_dedicated_usage))"),
      spec == "con"  ~ paste0(dv, " ~ ", "pre(abnb_dedicated_usage)"),
      spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(abnb_dedicated_usage))"),
      TRUE ~ "ERROR")) %>%
  run_dpm(dpm_quarter)

save(dpm_quarter_dedicated_usage_fit, file = "./data/output/dpm_alt_specs/dpm_quarter_dedicated_usage_fit.RData")

# AIRBNB NNI
# This adds an interaction with nearest neighbor index to test whether Airbnb effects are conditional on clustering.

dpm_quarter_nni_fit <- expand.grid(dv = c("dp_robbery", "dp_burglary", "dp_asb", "dp_violence", "dlg_violence_harm", "dlg_theft"),
                                     spec = c("both", "con", "lag")) %>%
  mutate(
    form = case_when(
      spec == "both" ~ paste0(dv, " ~ ", "pre(abnb_active_rentals) + pre(lag(abnb_active_rentals)) + pre(nni_rentals) + pre(lag(nni_rentals)) + pre(nni) + pre(lag(nni))"),
      spec == "con"  ~ paste0(dv, " ~ ", "pre(abnb_active_rentals) + pre(nni_rentals) + pre(nni)"),
      spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(abnb_active_rentals)) + pre(lag(nni_rentals)) + pre(lag(nni))"),
      TRUE ~ "ERROR")) %>%
  run_dpm(dpm_quarter)

summary(dpm(dp_robbery ~ pre(abnb_active_rentals) + pre(nni_rentals) + pre(nni), data = dpm_quarter))

save(dpm_quarter_nni_fit, file = "./data/output/dpm_alt_specs/dpm_quarter_nni_fit.RData")

# AIRBNB RATE
# This uses crime rates instead of counts as an outcome. It makes no difference
# but people might ask about it.

dpm_quarter_fit_rate <- expand.grid(dv = c("dp_robbery_rate", "dp_burglary_rate", "dp_asb_rate", "dp_violence_rate", "dlg_violence_harm_rate", "dlg_theft_rate"),
                                    spec = c("both", "con", "lag")) %>%
  mutate(
    form = case_when(
      spec == "both" ~ paste0(dv, " ~ ", "pre(abnb_active_rentals) + pre(lag(abnb_active_rentals))"),
      spec == "con"  ~ paste0(dv, " ~ ", "pre(abnb_active_rentals)"),
      spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(abnb_active_rentals))"),
      TRUE ~ "ERROR")) %>%
  run_dpm()

save(dpm_quarter_fit_rate, file = "./data/output/dpm_alt_specs/dpm_quarter_fit_rate.RData")

# AIRBNB SPATIAL LAG
# This adds the spatial lag of Airbnb properties. Spatial lags aren't usually 
# particularly predictive except burglary has a strong immediate positive, 
# lagged negative in combined model but neither notable when separate. Might be 
# illusory due to overpartitioning variance, but also might be legitimate 
# opposite effects. Hard to imagine mechanism for opposite signs. Has no impact
# on local effect either. Big CIs.

dpm_quarter_splag_fit <- expand.grid(dv = c("dp_robbery", "dp_burglary", "dp_asb", "dp_violence", "dlg_violence_harm", "dlg_theft"),
                               spec = c("both", "con", "lag")) %>%
  mutate(
    form = case_when(
      spec == "both" ~ paste0(dv, " ~ ", "pre(abnb_active_rentals) + pre(lag(abnb_active_rentals)) + pre(splag_abnb_active_rentals) + pre(lag(splag_abnb_active_rentals))"),
      spec == "con"  ~ paste0(dv, " ~ ", "pre(abnb_active_rentals) + pre(splag_abnb_active_rentals)"),
      spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(abnb_active_rentals)) + pre(lag(splag_abnb_active_rentals))"),
      TRUE ~ "ERROR")) %>%
  run_dpm()

save(dpm_quarter_splag_fit, file = "./data/output/dpm_quarter_splag_fit.RData")

# SPATIOTEMPORAL LAG
# DO NOT RUN: THIS WORKS BUT WILL PRODUCE OVER 2 GIGS OF OUTPUT
# dpm_quarter_sptlag_fit <- expand.grid(dv = c("dp_robbery", "dp_burglary", "dp_asb", "dp_violence", "dlg_violence_harm", "dlg_theft"),
#                                spec = c("both", "con", "lag")) %>%
#   mutate(
#     form = case_when(
#       spec == "both" ~ paste0(dv, " ~ ", "pre(abnb_active_rentals) + pre(lag(abnb_active_rentals))"),
#       spec == "con"  ~ paste0(dv, " ~ ", "pre(abnb_active_rentals)"),
#       spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(abnb_active_rentals))"),
#       TRUE ~ "ERROR")) %>%
#   run_dpm()
# 
# save(dpm_quarter_sptlag_fit, file = "./data/output/dpm_quarter_sptlag_fit.RData")

# MEDIAN PROPERTY VALUES
dpm_quarter_rpp_median_priceper_fit <- expand.grid(dv = c("dp_robbery", "dp_burglary", "dp_asb", "dp_violence", "dlg_violence_harm", "dlg_theft"),
                                     spec = c("both", "con", "lag")) %>%
  mutate(
    form = case_when(
      spec == "both" ~ paste0(dv, " ~ ", "pre(abnb_active_rentals) + pre(lag(abnb_active_rentals)) + pre(rpp_median_priceper) + pre(lag(rpp_median_priceper))"),
      spec == "con"  ~ paste0(dv, " ~ ", "pre(abnb_active_rentals) + pre(rpp_median_priceper)"),
      spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(abnb_active_rentals)) + pre(lag(rpp_median_priceper))"),
      TRUE ~ "ERROR")) %>%
  run_dpm()

save(dpm_quarter_rpp_median_priceper_fit, file = "./data/output/dpm_quarter_rpp_median_priceper_fit.RData")

# MEDIAN PROPERTY VALUES INTERACTED WITH AIRBNB
dpm_quarter_prop_price_rentals_fit <- expand.grid(dv = c("dp_robbery", "dp_burglary", "dp_asb", "dp_violence", "dlg_violence_harm", "dlg_theft"),
                                                   spec = c("both", "con", "lag")) %>%
  mutate(
    form = case_when(
      spec == "both" ~ paste0(dv, " ~ ", "pre(abnb_active_rentals) + pre(lag(abnb_active_rentals)) + pre(prop_price_rentals) + pre(lag(prop_price_rentals))"),
      spec == "con"  ~ paste0(dv, " ~ ", "pre(abnb_active_rentals)  + pre(prop_price_rentals)"),
      spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(abnb_active_rentals)) + pre(lag(prop_price_rentals))"),
      TRUE ~ "ERROR")) %>%
  run_dpm()

save(dpm_quarter_prop_price_rentals_fit, file = "./data/output/dpm_quarter_prop_price_rentals_fit.RData")

