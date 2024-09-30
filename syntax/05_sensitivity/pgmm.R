library(tidyverse)
library(plm)

# Yeah, system gets real angry at my trying to do PLM models with these data
# because of the number of time periods. Results look like standard FE panels
# anyway as well, though there's indication of autocorrelation and instrument
# problems.

load("./data/analytical/lsoa_quarter.RData")

pdata <- lsoa_quarter %>%
  group_by(lsoa_code) %>%
  arrange(date) %>%
  mutate(dp_robbery_diff = dp_robbery - dplyr::lag(dp_robbery),
         abnb_active_rentals_diff = abnb_active_rentals - dplyr::lag(abnb_active_rentals),
         lag_abnb_active_rentals_diff = dplyr::lag(abnb_active_rentals_diff)) %>%
  ungroup() %>%
  select(lsoa_code, date, dp_robbery_diff, abnb_active_rentals_diff, lag_abnb_active_rentals_diff) %>%
  pdata.frame(index=c("lsoa_code", "date"))


pgmm_fit <- pgmm(dp_robbery ~ lag(dp_robbery, 1:2) + lag(abnb_active_rentals, 0:1) | lag(dp_robbery, 2:99), data = pdata.frame(lsoa_month_props_crime, index=c("lsoa_code", "date")))
summary(pgmm_fit)
