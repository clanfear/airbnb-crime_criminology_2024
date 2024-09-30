# One potential threat to our analyses is the presence of events that strongly
# predict both short-term rentals and crime in London. Football matches are the
# most likely candidate---nothing else is likely to as strongly predict both and
# occur as frequently. Suggestions for data and how to approach this were
# provided by Kivan Polimis (Arsenal), Neil Trettin (Liverpool), Steven Karceski
# (Fulham).

# Derby games: 
# Arsenal vs. Tottenham Hotspur
# Arsenal vs. Chelsea
# Chelsea vs. Tottenham
# Millwall vs. West Ham
# Charlton vs. Millwall vs. Wimbledon
# Brentford vs. Chelsea vs. Fulham vs. Queens Park
# Also rivalry of Man U vs. Chelsea

# Any time a big team comeS: Man City, Liverpool, Man U

# Stadiums change during study period
# West Ham and Tottenham expanded notably.


# Data from https://datahub.io/sports-data/english-premier-league

library(tidyverse)
library(sf)
library(lubridate)
library(dpm)

load("./data/derived/shape/london_lsoa.RData")
load("./data/derived/neighbors/london_lsoa_neighbors.RData")
load("./data/analytical/lsoa_quarter.RData")
load("./data/analytical/lsoa_month.RData")

london_stadiums <- read_csv("./data/raw/football/stadiums-with-GPS-coordinates.csv") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(st_crs(london_lsoa)) %>%
  st_filter(london_lsoa) %>%
  st_join(london_lsoa %>% select(lsoa_code, geometry)) %>% 
  st_drop_geometry()

london_stadiums_1k <- read_csv("./data/raw/football/stadiums-with-GPS-coordinates.csv") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(st_crs(london_lsoa)) %>%
  st_join(london_lsoa %>% select(lsoa_code, geometry), join = st_is_within_distance, dist = units::set_units(1000, "m")) %>% 
  st_drop_geometry()

london_epl_games <- tibble(file = str_subset(list.files("./data/raw/football/sports-data_epl/data/", full.names = TRUE), "csv")) %>%
  mutate(season = str_extract(file, "[0-9]{4}"),
         start_year = str_sub(season, 1, 2)) %>%
  filter(as.numeric(start_year) >= 14 & as.numeric(start_year) <= 17) %>%
  mutate(data = list(map_dfr(file, ~read_csv(.x) %>% 
                               select(date = Date, home = HomeTeam, away = AwayTeam) %>% 
                               mutate(date = as.character(date))))) %>%
  unnest(data) %>%
  select(-file) %>%
  mutate(date = dmy(date)) %>%
  inner_join(london_stadiums, by = c("home" = "FDCOUK")) %>%
  mutate(rivalry = case_when(
    home %in% c("Arsenal", "Tottenham", "Chelsea") & away %in% c("Arsenal", "Tottenham", "Chelsea")               ~ "rivalry",
    home %in% c("Millwall", "West Ham") & away %in% c("Millwall", "West Ham")                                     ~ "rivalry",
    home %in% c("Charlton", "Millwall", "Wimbledon") & away %in% c("Charlton", "Millwall", "Wimbledon")           ~ "rivalry",
    home == "Chelsea" & away == "Man United"                                                                      ~ "rivalry",
    home %in% c("Brentford", "Chelsea", "Fulham" , "QPR") & away %in% c("Brentford", "Chelsea", "Fulham" , "QPR") ~ "rivalry",
    TRUE                                                                                                          ~ "normal"
  ),
  big_visitor = ifelse(away %in% c("Man City", "Man United", "Liverpool"), "big visitor", "normal"), 
  date_quarter = quarter(date, type = "date_first"),
  date_month = ym(paste(year(date), month(date), sep = "-")))

london_epl_month <- london_epl_games %>%
  filter(date_month >= min(lsoa_month_props_crime$date)  & date_month <= max(lsoa_month_props_crime$date)) %>%
  group_by(lsoa_code, date_month) %>%
  summarize(n_games       = n(),
            total_cap     = sum(Capacity),
            n_rivalry     = sum(rivalry == "rivalry"),
            n_big_visitor = sum(big_visitor == "big visitor"), 
            .groups       = "drop") %>%
  complete(lsoa_code  = unique(london_lsoa$lsoa_code), 
           date_month = unique(lsoa_month_props_crime$date), 
           fill       = list(n_games = 0, total_cap = 0, n_rivalry = 0, n_big_visitor = 0))

lsoa_month_props_crime_epl <- lsoa_month_props_crime %>%
  inner_join(london_epl_month, by = c("lsoa_code", "date" = "date_month"))


london_epl_quarter <- london_epl_games %>%
  filter(date_quarter >= min(lsoa_quarter$date) & date_quarter <= max(lsoa_quarter$date)) %>%
  group_by(lsoa_code, date_quarter) %>%
  summarize(n_games       = n(),
            total_cap     = sum(Capacity),
            n_rivalry     = sum(rivalry == "rivalry"),
            n_big_visitor = sum(big_visitor == "big visitor"), 
            .groups       = "drop") %>%
  complete(lsoa_code    = unique(london_lsoa$lsoa_code), 
           date_quarter = unique(lsoa_quarter$date), 
           fill = list(n_games = 0, total_cap = 0, n_rivalry = 0, n_big_visitor = 0))

lsoa_quarter_props_crime_epl <- lsoa_quarter %>%
  inner_join(london_epl_quarter, by = c("lsoa_code", "date" = "date_quarter"))


# Quarters
dpm_quarter_epl <- lsoa_quarter_props_crime_epl %>% 
  mutate(across(matches("(dp_|dlg_|abnb_|nni|rpp_|n_games|total_cap|n_big_visitor)"), ~standardize(.))) %>%
  mutate(date_num = as.numeric(year_quarter_fac)) %>%
  panelr::panel_data(id = lsoa_code, wave = date_num)

dpm_quarter_epl_fit <- expand.grid(dv = c("dp_robbery", "dp_burglary", "dp_asb", "dp_violence", "dlg_violence_harm", "dlg_theft"),
                               spec = c("both", "con", "lag")) %>%
  mutate(
    form = case_when(
      spec == "both" ~ paste0(dv, " ~ ", "pre(abnb_active_rentals) + pre(lag(abnb_active_rentals)) + n_games + lag(n_games)"),
      spec == "con"  ~ paste0(dv, " ~ ", "pre(abnb_active_rentals) + n_games"),
      spec == "lag"  ~ paste0(dv, " ~ ", "pre(lag(abnb_active_rentals)) + lag(n_games)"),
      TRUE ~ "ERROR")) %>%
  mutate(models = map(form, ~ dpm(formula(.x), data = dpm_quarter_epl, estimator = "MLM"))) %>%
  mutate(fit    = map(models, ~ lavaan::fitmeasures(.x))) %>% 
  mutate(RMSEA  = map_dbl(fit, ~.x["rmsea"]),
         SRMR   = map_dbl(fit, ~.x["srmr"]),
         BIC    = map_dbl(fit, ~.x["bic"]),
         N      = map_dbl(fit, ~.x["ntotal"])) %>%
  mutate(coefs  =  map(models, ~broom::tidy(.x, conf.int = TRUE))) %>% 
  select(-fit, -form) %>%
  unnest(coefs)

# Just checking excluding areas near stadiums
dpm(dp_robbery ~ pre(abnb_active_rentals) + pre(lag(abnb_active_rentals)), 
    data = dpm_quarter_epl %>% 
      filter(!lsoa_code %in% london_stadiums_1k$lsoa_code) %>%
      panelr::panel_data(id = lsoa_code, wave = date_num), estimator = "MLM") %>% summary()

# What about FE Poisson?
fixest::fepois(dp_robbery ~ abnb_active_rentals + n_games  | lsoa_code + date, data = lsoa_quarter_props_crime_epl)
fixest::fepois(dp_robbery ~ abnb_active_rentals + total_cap  | lsoa_code + date, data = lsoa_quarter_props_crime_epl)
