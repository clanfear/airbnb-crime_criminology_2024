# This script contains code to get assorted values found in the manuscript. A
# better way to do this would to have them all reproducibly inserted into the
# draft, but we were working in a Word doc by the time reviewers wanted these.
library(tidyverse)
library(sf)
library(ggtext)
library(patchwork)

load("./data/analytical/lsoa_quarter.RData")

load("./data/derived/london_lsoa_pop.RData")


abnb_households <- lsoa_quarter %>%
  inner_join(london_lsoa_pop) %>%
  group_by(lsoa_code) %>%
  mutate(abnb_dedicated_hh_density = as.numeric(abnb_dedicated_prop / households),
         abnb_hh_density_change = abnb_dedicated_hh_density - lag(abnb_dedicated_hh_density)) %>%
  ungroup() %>%
  select(lsoa_code, year_quarter, abnb_dedicated_hh_density, abnb_hh_density_change, abnb_dedicated_prop, households)

# Highest quarterly dedicated Airbnb density
abnb_households%>%
  arrange(desc(abnb_dedicated_hh_density))

# Highest change in density
abnb_households%>%
  arrange(desc(abs(abnb_hh_density_change)))

# Highest density LSOA
abnb_households %>%
  filter(lsoa_code == "E01004763") %>%
  print(n=100)

# Quartiles of density
abnb_households %>%
  group_by(lsoa_code) %>%
  summarize(abnb_dedicated_hh_density = mean(abnb_dedicated_hh_density)) %>%
  pull(abnb_dedicated_hh_density) %>%
  quantile(probs = c(1, 0.99, 0.95, 0.90, 0.75, 0.5))

# Quartiles of density change
abnb_households %>%
  group_by(lsoa_code) %>%
  pull(abnb_hh_density_change) %>%
  abs() %>%
  quantile(probs = c(1, 0.99, 0.95, 0.90, 0.75, 0.5), na.rm=TRUE)

# Unstandardized estimates
load("./data/output/dpm_quarter_fit_unstd.RData")
unstd_estimates <- dpm_quarter_fit_unstd |>
  filter(spec == "con" & term == "abnb_active_rentals") |>
  select(name = dv, est_effect = estimate)

# Different ways to interpret the unstandardized estimates
lsoa_quarter |> 
  filter(year == 2018) |> 
  group_by(lsoa_code) |> 
  summarize(across(c(dp_robbery, dp_burglary, dlg_theft, dp_violence), ~sum(., na.rm=TRUE))) |>
  pivot_longer(-lsoa_code) |>
  group_by(name) |>
  summarize(crime = sum(value)) |>
  left_join(unstd_estimates) |>
  mutate(yearly_effect = est_effect*4,
         effect_as_pct = yearly_effect/(crime/4835),
         active_props = 4835*15.40248,
         crime_1pct = crime*0.01,
         abnbs_to_get_1pct = (crime_1pct / yearly_effect)/4835,
         abnbs_pct_to_get_1pct = (abnbs_to_get_1pct+15.40248) / 15.40248,
         crime_from_10pctabnb = active_props*.1*yearly_effect,
         crimepct_from_10pctabnb = (crime_from_10pctabnb+crime)/crime) |> glimpse()

# Average active airbnbs per quarter
lsoa_quarter |> 
  group_by(year_quarter) |>
  summarize(abnb_active_rentals = sum(abnb_active_rentals)) |>
  pull(abnb_active_rentals) |> mean()
