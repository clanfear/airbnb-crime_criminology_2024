# This script processes publicly available data on crime in London. The data
# are large, however, so they get tossed in the secure data directory as well.

library(tidyverse)
library(vroom)
library(lubridate)
source("./syntax/project_functions.R")
load("./data/derived/shape/london_lsoa.RData")
load("./data/derived/shape/london_ward.RData")

# Manually change for your purposes.
data_dir <- "Z:/CSI Projects/Airbnb/Data/"

# data.london.gov data are only identified to the LSOA level
dlg_crime <- read_csv(paste0(data_dir, "data-london-gov-uk/mps_lsoa_level_crime_historic.csv"))|> 
  mutate(across(everything(), ~as.character(.))) |>
  pivot_longer(matches("\\d{6}")) |>
  janitor::clean_names()

# data.police.uk has lat/long with 0.951% of cases missing in crimes of interest
dp_crime <- vroom::vroom(str_subset(list.files(paste0(data_dir, "data-police-uk/"), full.names = TRUE), "csv$")) %>%
  janitor::clean_names() %>%
  mutate(date = lubridate::ym(month),
         row_id = row_number())

# Take the measures we want for cases with nonmissing lat/lon and within dates
dp_crime_lat_lon <- dp_crime |>
  filter(!is.na(longitude) & !is.na(latitude)) |>
  filter(date >= lubridate::ym("2014-01") & date <= lubridate::ym("2019-01")) |>
  filter(crime_type %in% c("Anti-social behaviour", "Robbery", "Violence and sexual offences", "Burglary")) |>
  mutate(crime_type = 
           case_when(crime_type == "Anti-social behaviour" ~ "dp_asb",
                     crime_type == "Robbery" ~ "dp_robbery",
                     crime_type == "Violence and sexual offences" ~ "dp_violence",
                     crime_type == "Burglary" ~ "dp_burglary",
                     TRUE ~ NA_character_))
save(dp_crime_lat_lon, file = "./data/derived/crime/dp_crime_lat_lon.RData")

# This index just records if each LSOA month is represented in each data set
lsoa_crime_index <- london_lsoa %>% 
  st_drop_geometry() %>%
  select(lsoa_code, borough_code) %>%
  mutate(year = 2014, month =1) %>%
  complete(nesting(lsoa_code, borough_code), year = 2014:2018, month = 1:12) %>%
  mutate(date = lubridate::ym(str_c(year, month, sep = "-")),
         in_dlg = lsoa_code %in% unique(dlg_crime$lsoa_code),
         in_dp  = lsoa_code %in% unique(dp_crime$lsoa_code))

# DATA.LONDON.GOV 
# Starting in 2017 there are missing values for some LSOAs in particular months.
# They're inconsistent but always the entire month for an LSOA, so maybe they
# just did not report for those months. FIML or whatnot can deal with that, 
# under assumption failure to report isn't because, say, they were all zero.
# There are LSOAs reporting zero for everything in a given month. Probably not 
# thee though.

# There also no observations for four of 6 City of London LSOAs

dlg_lsoa_crime_count <- dlg_crime %>%
  mutate(value = as.numeric(ifelse(value == "#N/A", NA, value)),
         year  = as.numeric(str_sub(name, 1, 4)),
         month = as.numeric(str_sub(name, 5, 6))) |>
  filter(year >= 2014) |>
  mutate(crime_type = 
           case_when(
             minor_category %in% c("Assault with Injury", "Murder", "Wounding/GBH")  ~ "dlg_violence_harm",
             minor_category %in% c("Common Assault", "Harassment", "Other violence") ~ "dlg_violence_noharm",
             minor_category == "Offensive Weapon"                                    ~ "dlg_weapon_possession",
             major_category == "Robbery"                                             ~ "dlg_robbery",
             major_category == "Burglary"                                            ~ "dlg_burglary",
             major_category == "Theft and Handling"                                  ~ "dlg_theft"
             )) |>
  filter(!is.na(crime_type)) |>
  group_by(lsoa_code, year, month, crime_type) |>
  summarize(n = sum(value), .groups = "drop")  |>
  pivot_wider(names_from = crime_type, values_from = n)

# For some reason the pasting and date conversion are causing mutate() to take
# an intolerably long time despite this being an instant operation done directly.
dlg_lsoa_crime_count$date <- lubridate::ym(str_c(dlg_lsoa_crime_count$year, 
                                                 dlg_lsoa_crime_count$month, sep = "-"))
dlg_lsoa_crime_count <- dlg_lsoa_crime_count %>% select(-year, - month)

# I can get back robbery and burglary from the dp data

# DATA.POLICE.UK
# These data are cleaner but only have the major categories.
# These are missing three seemingly random LSOAs in Lambeth, Tower Hamlets, and Islington.
# I use the dlg data to get these back, though they have no antisocial behavior.

dp_lsoa_crime_count <- dp_crime |>
  filter(!is.na(lsoa_code)) |>
  filter(date >= lubridate::ym("2014-01") & date <= lubridate::ym("2019-01")) |>
  filter(crime_type %in% c("Anti-social behaviour", "Robbery", "Violence and sexual offences", "Burglary")) |>
  count(lsoa_code, date, crime_type) |>
  complete(lsoa_code, date, crime_type, fill = list(n = 0)) |>
  mutate(crime_type = 
           case_when(crime_type == "Anti-social behaviour" ~ "dp_asb",
                     crime_type == "Robbery" ~ "dp_robbery",
                     crime_type == "Violence and sexual offences" ~ "dp_violence",
                     crime_type == "Burglary" ~ "dp_burglary",
                     TRUE ~ NA_character_)) |>
  pivot_wider(names_from = crime_type, values_from = n) 

# Combine them together
lsoa_crime_count <- lsoa_crime_index |>
  left_join(dlg_lsoa_crime_count) |>
  left_join(dp_lsoa_crime_count) |>
  mutate(dlg_burglary = ifelse(is.na(dlg_burglary), dp_burglary, dlg_burglary),
         dlg_robbery  = ifelse(is.na(dlg_robbery), dp_robbery, dlg_robbery),
         dp_burglary  = ifelse(is.na(dp_burglary), dlg_burglary, dp_burglary),
         dp_robbery   = ifelse(is.na(dp_robbery), dlg_robbery, dp_robbery),
         dp_violence  = ifelse(is.na(dp_violence), dlg_violence_noharm + dlg_violence_harm, dp_violence))

# Check how the two data sets are correlated (very high)
lsoa_crime_count %>%
  select(matches("^(dlg|dp)")) %>%
  cor(use = "pairwise.complete")

save(lsoa_crime_count, file = "./data/derived/crime/lsoa_crime_count.RData")
