# Ke et al. used Airbnb reviews as a proxy for usage. I was curious how similar
# the AirDNA usage measure is to review counts, so I did a quick comparison.
# Turns out reviews are a poor measure of usage for individual properties but
# a great measure at the LSOA level, at least on average. Probably breaks down
# in locations with few Airbnbs.
library(tidyverse)
load("./data/derived/airbnb/airdna_lsoa_disagg.RData")

data_dir <- "Z:/CSI Projects/Airbnb/Data/CDRC/Data/"
airdna_reviews <- vroom::vroom(paste0(data_dir, "safeguarded.airdna.Review.csv"))
prop_reviews <- airdna_reviews |>
  janitor::clean_names() |>
  filter(city_extract == "London") |>
  mutate(year = lubridate::year(review_date)) |>
  count(property_id, year, name = "reviews")

# Do correlation at property level then correlation at LSOA and ward levels

load("./data/derived/airbnb/airdna_monthly.RData")
airdna_reviews_usage <- airdna_lsoa_disagg %>% 
  mutate(year = lubridate::year(date)) %>%
  rename(property_id = abnb_property_id) %>%
  group_by(property_id, year, lsoa_code) %>%
  summarize(usage = sum(abnb_usage, na.rm=TRUE),
            usage_consv = sum(abnb_usage_consv, na.rm=TRUE), .groups = "drop") %>%
  inner_join(prop_reviews)
list_missing(airdna_reviews_usage)

# Only .46 to .51 corr between reviews and usage at property level
airdna_reviews_usage %>% select(-property_id, - year) %>% cor()

# .93 to .94 corr between reviews and usage at LSOA level; bit lower with props
airdna_reviews_usage %>%
  select(-property_id) %>%
  group_by(lsoa_code, year) %>%
  summarize(across(everything(), ~sum(.)),
            props = n(), .groups = "drop") %>%
  select(-lsoa_code, -year) %>% cor()
