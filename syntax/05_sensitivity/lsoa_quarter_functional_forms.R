# As a precaution, it is a good idea to explore if there are nonlinear relationships
# between Airbnbs and our crime outcomes. This just tries some alternate functional
# forms. Unfortunately, there are limits to the ability of the modeling approach
# to handle more predictors, particularly skewed ones, and also challenges to
# diagnosing model fit and functional forms.

library(tidyverse)
library(fixest)
library(mgcv)
library(panelr)
library(dpm)
library(DHARMa)
source("./syntax/project_functions.R")

load("./data/analytical/lsoa_quarter_props_crime.RData")
list_missing(lsoa_quarter_props_crime)


# Quarters
dpm_quarter <- lsoa_quarter_props_crime %>%
  mutate(dlg_violence = dlg_violence_noharm + dlg_violence_harm) %>%
  mutate(across(matches("abnb"), ~ poly(., 2)[,1], .names = "{.col}_1"),
         across(c(matches("abnb"), -matches("_1$")), ~ poly(., 2)[,2], .names = "{.col}_2")) %>%
  mutate(across(matches("^(dp|dlg|abnb|nni|rpp)"), ~standardize(.))) %>%
  mutate(date_num = as.numeric(year_quarter_fac)) %>%
  panelr::panel_data(id = lsoa_code, wave = date_num)

lsoa_quarter <- lsoa_quarter_props_crime %>%
  mutate(dlg_violence = dlg_violence_noharm + dlg_violence_harm) %>%
  mutate(across(matches("^(abnb|nni|rpp)"), ~standardize(.)),
         across(matches("abnb"), ~ poly(., 2)[,1], .names = "{.col}_1"),
         across(c(matches("abnb"), -matches("_1$")), ~ poly(., 2)[,2], .names = "{.col}_2"),
         across(c(matches("abnb"), -matches("_1$")), ~ poly(., 2)[,2], .names = "{.col}_3")) %>%
  mutate(date_num = as.numeric(year_quarter_fac))

# DPM
# Not really any strong evidence for quadratic relationship; can't likely fit a cubic
dpm(dp_robbery ~ pre(abnb_active_rentals_1) + pre(abnb_active_rentals_2), data = dpm_quarter, estimator = "MLR") %>% summary()
dpm(dp_burglary ~ pre(abnb_active_rentals_1) + pre(abnb_active_rentals_2), data = dpm_quarter, estimator = "MLR") %>% summary()

# FIXEST
fepois(c(dp_robbery, dp_burglary, dp_asb, dp_violence, dlg_violence_harm, dlg_theft)  ~ 
         abnb_active_rentals + l(abnb_active_rentals) |  
         lsoa_code + date_num, 
       data = lsoa_quarter,
       panel.id = c("lsoa_code", "date_num"),
       vcov = "DK")

fepois(dlg_violence_harm  ~ 
         abnb_active_rentals_1 +  abnb_active_rentals_2  |  
         lsoa_code + date_num, 
       data = lsoa_quarter,
       panel.id = c("lsoa_code", "date_num"))

ols_robbery <- feols(dp_robbery  ~ 
                         abnb_active_rentals + l(abnb_active_rentals) |  
                         lsoa_code + date_num, 
                       data = lsoa_quarter %>% mutate(dp_robbery = log(dp_robbery + 1)),
                       panel.id = c("lsoa_code", "date_num"))
data.frame(fitted = predict(pois_robbery, type = "response"),
           resid  = residuals(pois_robbery, type = "response")) %>%
  ggplot(aes(x = fitted, y = resid)) + geom_point() + geom_smooth()

# PANELR WITHIN BETWEEN SO I CAN DHARMA THAT SHIT

burg_wbm_pois <- wbm(dp_burglary ~ abnb_active_rentals + lag(abnb_active_rentals), data = dpm_quarter, family = poisson(link = "log"))
sim_res <- simulateResiduals(burg_wbm_pois)
burg_wbgee_pois <- wbgee(dp_burglary ~ abnb_active_rentals + lag(abnb_active_rentals), data = dpm_quarter, family = poisson(link = "log"))
summary(burg_wbgee_pois)
