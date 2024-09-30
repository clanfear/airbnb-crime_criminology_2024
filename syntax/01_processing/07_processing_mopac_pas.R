# This script processes the MOPAC Public Attitudes Survey data. The PAS changes
# regularly, so each version gets a bit of processing.

library(tidyverse)
library(haven)
source("./syntax/project_functions.R")
load("./data/derived/ward_lsoa_index.RData")

# Manual directory setting again
data_dir <- "Z:/CSI Projects/Airbnb/Data/"

# Financial year 11 is 2015; 13 is 2017 etc.
# Data are timed by financial year and quarter with no indication how quarter lines up with anything
# MOPAC financial docs indicate the 2015/16 year ends March 31st 3016
# This suggests the first quarter (41) of 2015/2016 is April-June 2015, last quarter  (44) is Jan-March 2016

# Airbnb data start Nov 2014 and go until May 2018
# MOPAC data start with quarter 2 2015, so we'll basically lose one quarter
# They don't survey the City of London; population is like 9000 and it is not
# under Metroplitan police jurisdiction.

pas_15_17_raw <- read_spss("Z:/CSI Projects/Airbnb/Data/MOPAC Public Attitudes Survey/Data/FY 15-17 PAS data ward date.sav")

# Perceived disorder questions have a lot of missingness starting with 2017-2018

pas_15_17 <- pas_15_17_raw %>%
  select(ward_code         = ward,
         ward_borough      = ward_unique,
         lsoa_code         = SOA1,
         msoa_code         = SOA2,
         interview_date,
         borough           = C2,
         race              = NQ147r,
         tenure            = Q1,
         age               = Q136r,
         employment        = Q139r,
         homeowner         = Q144,
         gender            = XQ135r,
         coh_trust         = Q3C, # People in this neighbourhood can be trusted
         coh_courtesy      = Q3F, # People act with courtesy to each other in public space in this area
         coh_take_pride    = Q3G, # You can see from the public space here in the area that people take pride in their environment
         coh_get_help      = Q3I, # If I sensed trouble whilst in this area, I could get help from people who live here
         coh_get_along     = Q3L, # ... this local area is a place where people from different backgrounds get on well together
         inf_have_control  = Q3H, # Local people/residents and authorities have control over the public space in this area
         inf_call_police   = Q3J, # The people who live here can be relied upon to call the police if someone is acting suspiciously
         inf_sanction      = Q3K, # If any of the children or young people around here are causing trouble, local people will tell them off
         percrime_violence = NNQ27E,
         percrime_gangs    = NQ43,
         percrime_gun      = Q37,
         percrime_knife    = Q39A_2,
         percrime_general  = NNQ27C,
         # perdis_asb        = Q15, # Not in later waves
         perdis_noise      = Q10A,
         perdis_teens      = Q10B,
         perdis_litter     = Q10C,
         perdis_vandalism  = Q10D,
         perdis_drunk      = Q10F,
         victim_year         = BQ90A,
         see_patrols         = Q65 
         )



pas_17_18_raw <- read_spss("Z:/CSI Projects/Airbnb/Data/MOPAC Public Attitudes Survey/Data/FY 17-18 PAS data ward date.sav")
pas_17_18 <- pas_17_18_raw %>%
  select(ward_code         = ward,
         ward_borough      = ward_unique,
         lsoa_code         = SOA1,
         msoa_code         = SOA2,
         interview_date,
         borough           = C2,
         race              = NQ147r,
         tenure            = Q1,
         age               = Q136r,
         employment        = Q139r,
         homeowner         = Q144,
         gender            = XQ135r,
         coh_trust         = Q3C,
         coh_courtesy      = Q3F,
         coh_take_pride    = Q3G,
         coh_get_help      = Q3I,
         coh_get_along     = Q3L,
         inf_have_control  = Q3H,
         inf_call_police   = Q3J,
         inf_sanction      = Q3K,
         percrime_violence = NNQ27E,
         percrime_gangs    = NQ43,
         percrime_gun      = Q37,
         percrime_knife    = Q39A_2,
         percrime_general  = NNQ27C,
         perdis_noise      = ZQ10A,
         perdis_teens      = ZQ10B,
         perdis_litter     = ZQ10C,
         perdis_vandalism  = ZQ10D,
         perdis_drunk      = ZQ10F,
         victim_year       = BQ90A,
         see_patrols         = Q65
  )

pas_18_19_raw <- read_spss("Z:/CSI Projects/Airbnb/Data/MOPAC Public Attitudes Survey/Data/FY 18-19 PAS data ward date.sav")

pas_18_19 <- pas_18_19_raw %>%
  select(ward_code         = ward,
         ward_borough      = ward_unique,
         lsoa_code         = SOA1,
         msoa_code         = SOA2,
         interview_date,
         borough           = C2,
         race              = NQ147r,
         tenure            = Q1,
         age               = Q136r,
         employment        = Q139r,
         homeowner         = Q144,
         gender            = XQ135r,
         coh_trust         = Q3C,
         coh_courtesy      = Q3F,
         coh_take_pride    = Q3G,
         coh_get_help      = Q3I,
         coh_get_along     = Q3L,
         inf_have_control  = Q3H,
         inf_call_police   = Q3J,
         inf_sanction      = Q3K,
         percrime_violence = NNQ27E,
         percrime_gangs    = NQ43,
         percrime_gun      = Q37,
         percrime_knife    = Q39A_2,
         percrime_general  = NNQ27C,
         perdis_noise      = ZQ10A,
         perdis_teens      = ZQ10B,
         perdis_litter     = ZQ10C,
         perdis_vandalism  = ZQ10D,
         perdis_drunk      = ZQ10F,
         victim_year       = BQ90A,
         see_patrols       = Q65
  )

# Create a wide dataset
pas_15_18_wide <- bind_rows(pas_15_17, pas_17_18, pas_18_19) |>
  # First, convert numeric quarter into date of start of quarter. Quarter 41 is
  # 4-Apr-2015, so we subtract 41 from quarter and multiply by 3 to get months
  # to add to get the right quarter date.
  mutate(id = factor(row_number()),
         month = lubridate::floor_date(interview_date, "month"),
         quarter = lubridate::floor_date(interview_date, "quarter"),
         year  = lubridate::year(interview_date),
         year_half = year + ifelse(lubridate::month(interview_date) <= 6, 0, 0.5),
  # Dispose of pesky NA values
         across(c(ward_code, ward_borough, lsoa_code, msoa_code), ~as.character(.)),
         across(c(borough, race, employment, gender, homeowner, victim_year), 
                ~ fct_recode(as_factor(.), NULL =  "Don't know", NULL = "Refused", NULL = "Not Asked")),
         across(c(matches("^(coh|inf|per)")), 
                ~ fct_rev(fct_recode(as_factor(., ordered = TRUE), NULL =  "Don't know", NULL = "Refused", NULL = "Not Asked"))),
         across(c(tenure, age), 
                ~ fct_recode(as_factor(., ordered = TRUE), NULL =  "Don't know", NULL = "Refused", NULL = "Not Asked"))) |>
  mutate(
    tenure_num = as.numeric(tenure),
    age_num = as.numeric(age),
    gender = na_if(gender, "Other"),
    homeowner = case_when(
      is.na(homeowner)                                         ~ NA_character_,
      homeowner %in% c("Owned outright", "Buying on mortgage") ~ "owner", 
      TRUE                                                     ~ "renter"),
    race = case_when(
      is.na(race)                   ~ NA_character_,
      race %in% c("Other", "Mixed") ~ "Other",
      TRUE                          ~ as.character(race)
    ),
    employment = fct_relevel(employment, "Unemployed"),
    see_patrols = fct_rev(as_factor(see_patrols))) |> 
  droplevels() |>
  filter(year <= 2018) |>
  # So Wards change a lot over time, so better to reconcile to a single set: 2018.
  rename(old_ward_code = ward_code) |>
  left_join(ward_lsoa_index) |>
  mutate(year_ward = paste0(year, "_", ward_code))

# Generally the reconciled ward is the same as the old ward, except for the one period where they used 2001 LSOAs
# I took a random sample of the ones from 2015-07-01 and sometimes the LSOA wasn't even in the old listed ward.
# This gives me some confidence in the reconciliation process---not sure how they assigned Wards but my way doesn't look less accurate.
pas_15_18_wide %>% filter(ward_code != old_ward_code & old_ward_code %in% ward_lsoa_index$ward_code) %>% count(quarter)

# Long version for use with hierarchical models
pas_15_18_long <- pas_15_18_wide %>%
  select(-old_ward_code) %>%
  mutate(across(matches("^(coh|inf|per)"), ~as.numeric(.))) %>%
  # coh_get_along has no middle category
  mutate(coh_get_along = ifelse(coh_get_along %in% 1:2, coh_get_along, coh_get_along + 1)) %>%
  pivot_longer(matches("^(coh|inf|per)"), names_to = "measure", values_to = "value")

save(pas_15_18_wide, file = "./data/derived/mopac/pas_15_18_wide.RData")
save(pas_15_18_long, file = "./data/derived/mopac/pas_15_18_long.RData")
