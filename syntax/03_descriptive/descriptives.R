library(tidyverse)
library(flextable)
library(fastDummies)
source("./syntax/project_functions.R")

load("./data/analytical/lsoa_month_props_crime.RData")
load("./data/analytical/lsoa_quarter_props_crime.RData")
load("./data/analytical/ward_half.RData")
load("./data/derived/mopac/pas_15_18_wide.RData")

sources <- c("abnb" = "AirDNA",
             "dlg"  = "data.london.gov",
             "dp"   = "data.police.uk",
             "rpp"  = "Resid. Price Data",
             "ce"  = "MOPAC PAS")

month_descriptives <- lsoa_month_props_crime %>%
  select(dp_robbery, dp_burglary, dp_asb, dp_violence, dlg_violence_harm, dlg_theft, abnb_active_rentals, abnb_entire_home_apt, 
         abnb_entire_home_apt, abnb_private_room, abnb_shared_room) %>% 
  pivot_longer(everything()) %>%
  group_by(name) %>%
  summarize(across(value, list( 
                              mean = ~mean(., na.rm=TRUE), 
                              median =  ~median(., na.rm=TRUE), 
                              min = ~min(., na.rm=TRUE),
                              max =  ~max(., na.rm=TRUE),
                              sd = ~sd(., na.rm=TRUE),
                              missing = ~sum(is.na(.)),
                              missing_pct = ~100*sum(is.na(.))/n()), 
                   .names = "{.fn}")) %>%
  mutate(source = sources[str_extract(name, "^[a-z]+")],
         .after = "name") %>%
  mutate(name = str_remove(name, "^[a-z]+_"))
save(month_descriptives, file = "./data/output/month_descriptives.RData")

quarter_descriptives <- lsoa_quarter_props_crime %>%
  select(dp_robbery, dp_burglary, dp_asb, dp_violence, dlg_violence_harm, dlg_theft, abnb_active_rentals,  
         abnb_entire_home_apt, abnb_private_room, abnb_shared_room) %>% 
  pivot_longer(everything()) %>%
  group_by(name) %>%
  summarize(across(value, list( 
                               mean = ~mean(., na.rm=TRUE), 
                               median =  ~median(., na.rm=TRUE),
                               min = ~min(., na.rm=TRUE),
                               max =  ~max(., na.rm=TRUE),
                               sd = ~sd(., na.rm=TRUE),
                               missing = ~sum(is.na(.)),
                               missing_pct = ~100*sum(is.na(.))/n()), 
                   .names = "{.fn}")) %>%
  mutate(source = sources[str_extract(name, "^[a-z]+")],
         .after = "name") %>%
  mutate(name = str_remove(name, "^[a-z]+_"))
save(quarter_descriptives, file = "./data/output/quarter_descriptives.RData")

year_half_descriptives <- ward_half %>%
  mutate(ce = standardize(ce)) |> # This is standardized in the models during immediate preprocessing
  select(dp_robbery, dp_burglary, dp_asb, dp_violence, dlg_violence_harm_aw, dlg_theft_aw, abnb_active_rentals, ce,
         abnb_entire_home_apt, abnb_private_room, abnb_shared_room, rpp_median_priceper_aw) %>% 
  pivot_longer(everything()) %>%
  group_by(name) %>%
  summarize(across(value, list( 
    mean = ~mean(., na.rm=TRUE), 
    median =  ~median(., na.rm=TRUE),
    min = ~min(., na.rm=TRUE),
    max =  ~max(., na.rm=TRUE),
    sd = ~sd(., na.rm=TRUE),
    missing = ~sum(is.na(.)),
    missing_pct = ~100*sum(is.na(.))/n()), 
    .names = "{.fn}")) %>%
  mutate(source = sources[str_extract(name, "^[a-z]+")],
         .after = "name") %>%
  mutate(name = str_replace(str_remove(name, "^[a-z]+_"), "^ce", "Collective Efficacy"))
save(year_half_descriptives, file = "./data/output/year_half_descriptives.RData")

pas_vars <- pas_15_18_wide %>%
  transmute(race, employment, tenure_num, age_num, homeowner, male = gender, victim_year, ward_year = paste0(ward_code, "_", year),
         coh_courtesy, coh_trust, coh_take_pride, coh_get_help, coh_get_along, inf_have_control, inf_call_police, inf_sanction) %>%
  mutate(across(c(homeowner, male, victim_year), ~ -as.numeric(as_factor(.)) + 2),
         across(matches("coh|inf"), ~as.numeric(.))) %>%
  dummy_cols(select_columns = c("race", "employment"), 
             remove_selected_columns = TRUE, 
             ignore_na = TRUE)

load("./data/output/pas_cfa_imputed_loadings.RData")
indicators <- c(
  get_along= "... this local area is a place where people from different backgrounds get on well together.",
  trust= "People in this neighbourhood can be trusted.",
  courtesy= "People act with courtesy to each other in public space in this area.",
  take_pride= "You can see from the public space here in the area that people take pride in their environment.",
  get_help= "If I sensed trouble whilst in this area, I could get help from people who live here.",
  have_control= "Local people/residents and authorities have control over the public space in this area.",
  call_police= "The people who live here can be relied upon to call the police if someone is acting suspiciously.",
  sanction= "If any of the children or young people around here are causing trouble, local people will tell them off.")

pas_cfa_descriptives <- pas_cfa_imputed_loadings %>%
  transmute(Indicator = indicators[str_remove(term, "^(coh_|inf_)")], 
            Estimate = round(estimate,3))

save(pas_cfa_descriptives, file = "./data/output/pas_cfa_descriptives.RData")

pas_descriptives <- pas_vars %>%
  pivot_longer(-ward_year) %>%
  group_by(name) %>%
  summarize(across(value, list( 
    mean = ~mean(., na.rm=TRUE), 
    median =  ~median(., na.rm=TRUE),
    min = ~min(., na.rm=TRUE),
    max =  ~max(., na.rm=TRUE),
    sd = ~sd(., na.rm=TRUE),
    missing = ~sum(is.na(.)),
    missing_pct = ~100*sum(is.na(.))/n()), 
    .names = "{.fn}")) %>%
  mutate(battery = case_when(
    str_detect(name, "^(coh_|inf_)") ~ "Collective Efficacy",
    str_detect(name, "^race_") ~ "Race",
    str_detect(name, "^employment_")~ "Employment",
    TRUE ~ ""),
    name = str_remove_all(name, "^(coh_|inf_|race_|employment_)|(_num|_year)")) %>%
  mutate(name = ifelse(battery == "Collective Efficacy", indicators[name], name)) %>%
  arrange(battery) %>%
  select(battery, Measure = name, mean, median, min, max, sd, missing, missing_pct)
save(pas_descriptives, file = "./data/output/pas_descriptives.RData")

# GENERATE TABLES VIA RMD

render("./syntax/03_descriptive/tables.Rmd", output_dir = "./docs/tables/", output_file = "tables.docx")
