# This script tests whether differences in coefficients for different property
# types are statistically significant or not using derived parameters in the SEM

library(tidyverse)
library(dpm)
source("./syntax/project_functions.R")

load("./data/analytical/lsoa_quarter.RData")
list_missing(lsoa_quarter)

# Quarters
dpm_quarter <- lsoa_quarter %>%
  mutate(dlg_violence = dlg_violence_noharm + dlg_violence_harm) %>%
  mutate(across(matches("^(dp|dlg|abnb|nni|rpp)"), ~standardize(.))) %>%
  mutate(date_num = as.numeric(year_quarter_fac)) %>%
  panelr::panel_data(id = lsoa_code, wave = date_num)

dpm_quarter_diffprops_form <- expand.grid(dv = c("dp_robbery", "dp_burglary", "dp_asb", "dp_violence", "dlg_violence_harm", "dlg_theft"),
                                         spec = c("both")) %>%
  mutate(form = paste0(dv, " ~ ", "pre(abnb_entire_home_apt) + pre(lag(abnb_entire_home_apt)) + pre(abnb_private_room) + pre(lag(abnb_private_room)) + pre(abnb_shared_room) + pre(lag(abnb_shared_room))"))


mod_fits <- map(dpm_quarter_diffprops_form$form, \(x){
  mod_fit <- dpm(formula(x), data = dpm_quarter, missing = "ML", estimator = "MLR")})


inequality_fits <- map(mod_fits, \(x){
  form_equalities <-  paste0(
    x@mod_string,
    "
\n
entire_v_private := en1 - en3
entire_v_shared := en1 - en5
private_v_shared := en3 - en5
"
  )
  mod_out <- sem(form_equalities, data = x@wide_data, missing = "ML", estimator = "MLR", fixed.x = FALSE, conditional.x = FALSE)
})

inequality_fits |> 
  map(\(x){
  pe_sum <- summary(x)$pe
  outcome <- pe_sum |> filter(label == "en1") |> slice(1L) |> pull(lhs) 
  pe_sum |> filter(op == ":=") |> mutate(out = outcome) |>
    mutate(across(where(is.numeric), ~round(., 3)))
  }) |>
  setNames(dpm_quarter_diffprops_form$dv)
