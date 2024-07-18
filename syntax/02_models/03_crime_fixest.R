library(tidyverse)
library(fixest, pos = 99999)
library(modelsummary)
source("./syntax/project_functions.R")
load("./data/analytical/dpm_ward_half.RData")
load("./data/analytical/lsoa_month_props_crime.RData")

fixest_month <- lsoa_month_props_crime %>%
  mutate(across(matches("^(dp|dlg|abnb|nni|rpp)"), ~standardize(.), .names = "{.col}_std"),
         across(matches("^(dp|dlg)"), ~standardize(log(. + 1)), .names = "{.col}_log"))

fixest_estimates <- list(
  "Linear Contemporaneous"         = feglm(dp_robbery_log ~ abnb_active_rentals_std | lsoa_code + date_num, data = fixest_month, panel.id = c("lsoa_code", "date_num")),
  "Linear Both"                    = feglm(dp_robbery_log ~ l(abnb_active_rentals_std, 0:1) | lsoa_code + date_num, data = fixest_month, panel.id = c("lsoa_code", "date_num")),
  "Dynamic Linear"                 = feglm(dp_robbery_log ~ l(dp_robbery_log) + l(abnb_active_rentals_std, 0:1) | lsoa_code + date_num, data = fixest_month, panel.id = c("lsoa_code", "date_num")),
  "Poisson Contemporaneous"         = fepois(dp_robbery ~ abnb_active_rentals_std | lsoa_code + date_num, data = fixest_month, panel.id = c("lsoa_code", "date_num")),
  "Poisson Both"                    = fepois(dp_robbery ~ l(abnb_active_rentals_std, 0:1) | lsoa_code + date_num, data = fixest_month, panel.id = c("lsoa_code", "date_num")),
  "Dynamic Poisson Both"            = fepois(dp_robbery ~ l(dp_robbery,1) + l(abnb_active_rentals_std, 0:1) | lsoa_code + date_num, data = fixest_month, panel.id = c("lsoa_code", "date_num"))
  )
fixest_table <- modelsummary::modelsummary(fixest_estimates, 
                                           stars = c("*" = 0.05), 
                                           output = "flextable", 
                                           gof_map = c("nobs", "AIC", "BIC"),
                                           coef_rename = function(x){
                                             case_when(
                                               x == "abnb_active_rentals_std" ~ "Active Rentals",
                                               x == "l(abnb_active_rentals_std, 1)" ~ "Active Rentals (T-1)",
                                               x == "l(dp_robbery, 1)" ~ "Robbery (T-1)",
                                               x == "l(dp_robbery_log, 1)" ~ "Log(Robbery) (T-1)",
                                               TRUE ~ "ERROR"
                                             )
                                           })
save(fixest_table, file = "./data/output/fixest_table.RData")

# While less important than monthly, we ran these at Ward half-year too
feols(dlg_theft_aw ~ std_abnb_active_rentals + std_collective_efficacy | ward_code + year_half, data = dpm_ward_half, panel.id = c("ward_code", "year_half"))
feols(dp_burglary ~ std_abnb_active_rentals + std_collective_efficacy | ward_code + year_half, data = dpm_ward_half, panel.id = c("ward_code", "year_half"))
fepois(dp_robbery ~ std_abnb_active_rentals + std_collective_efficacy | ward_code + year_half, data = dpm_ward_half, panel.id = c("ward_code", "year_half"))

feols(std_collective_efficacy ~ std_abnb_active_rentals + l(std_abnb_active_rentals)  | ward_code + year_half, data = dpm_ward_half, panel.id = c("ward_code", "year_half"))
feols(std_abnb_active_rentals ~ std_collective_efficacy + l(std_collective_efficacy)  | ward_code + year_half, data = dpm_ward_half, panel.id = c("ward_code", "year_half"))
fepois(abnb_active_rentals ~ std_collective_efficacy + l(std_collective_efficacy)  | ward_code + year_half, data = dpm_ward_half, panel.id = c("ward_code", "year_half"))
