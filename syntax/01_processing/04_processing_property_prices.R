library(data.table)

data_dir <- "Z:/CSI Projects/Airbnb/Data/property_prices/"

property_prices <- fread(paste0(data_dir, "tranall2011_19.csv"))[ , .(lsoa_code = lsoa11, postcode, dateoftransfer, price, tfarea, priceper)]

property_prices <- property_prices[dateoftransfer >= lubridate::ymd("2014-01-01") & dateoftransfer < lubridate::ymd("2019-01-01")]
property_prices <- property_prices[, `:=`(date    =  lubridate::ym(format(as.Date(dateoftransfer), "%Y-%m")),
                                          quarter =  lubridate::quarter(dateoftransfer, type = "date_first"),
                                          year_half = lubridate::year(dateoftransfer) + (0.5 * (lubridate::month(dateoftransfer) >= 7)),
                                          year    =  lubridate::year(dateoftransfer))]

property_prices_lsoa_month <- property_prices[, 
                .(rpp_n_sales         = .N, 
                  rpp_mean_price      = mean(price), 
                  rpp_median_price    = median(price), 
                  rpp_mean_area       = mean(tfarea),
                  rpp_median_area     = median(tfarea),
                  rpp_mean_priceper   = mean(priceper), 
                  rpp_median_priceper = median(priceper)),
                by = .(lsoa_code, date)][order(lsoa_code, date)] 

# Make implicit missings explicit. No sales are observed in LSOA "E01002907" so I manually add it back in.
property_prices_lsoa_month <- property_prices_lsoa_month %>%
  complete(lsoa_code = c(unique(property_prices$lsoa_code),"E01002907"), date, fill = list(rpp_n_sales = 0)) %>%
  mutate(rpp_n_sales = ifelse(lsoa_code == "E01002907", NA, rpp_n_sales))

save(property_prices_lsoa_month, file = "./data/derived/property_prices/property_prices_lsoa_month.RData")

# QUARTER
property_prices_lsoa_quarter <- property_prices[, 
                                              .(rpp_n_sales = .N, 
                                                rpp_mean_price      = mean(price), 
                                                rpp_median_price    = median(price), 
                                                rpp_mean_area       = mean(tfarea),
                                                rpp_median_area     = median(tfarea),
                                                rpp_mean_priceper   = mean(priceper), 
                                                rpp_median_priceper = median(priceper)),
                                              by = .(lsoa_code, quarter)][order(lsoa_code, quarter)]

property_prices_lsoa_quarter <- property_prices_lsoa_quarter %>%
  complete(lsoa_code = c(unique(property_prices$lsoa_code),"E01002907"), quarter, fill = list(rpp_n_sales = 0)) %>%
  mutate(rpp_n_sales = ifelse(lsoa_code == "E01002907", NA, rpp_n_sales))

save(property_prices_lsoa_quarter, file = "./data/derived/property_prices/property_prices_lsoa_quarter.RData")

# HALF-YEAR

property_prices_lsoa_half <- property_prices[, 
                                                .(rpp_n_sales = .N, 
                                                  rpp_mean_price      = mean(price), 
                                                  rpp_median_price    = median(price), 
                                                  rpp_mean_area       = mean(tfarea),
                                                  rpp_median_area     = median(tfarea),
                                                  rpp_mean_priceper   = mean(priceper), 
                                                  rpp_median_priceper = median(priceper)),
                                                by = .(lsoa_code, year_half)][order(lsoa_code, year_half)]

property_prices_lsoa_half <- property_prices_lsoa_half %>%
  complete(lsoa_code = c(unique(property_prices$lsoa_code),"E01002907"), year_half, fill = list(rpp_n_sales = 0)) %>%
  mutate(rpp_n_sales = ifelse(lsoa_code == "E01002907", NA, rpp_n_sales))

save(property_prices_lsoa_half, file = "./data/derived/property_prices/property_prices_lsoa_half.RData")

# YEAR
property_prices_lsoa_year <- property_prices[, 
                                                .(rpp_n_sales = .N, 
                                                  rpp_mean_price      = mean(price), 
                                                  rpp_median_price    = median(price), 
                                                  rpp_mean_area       = mean(tfarea),
                                                  rpp_median_area     = median(tfarea),
                                                  rpp_mean_priceper   = mean(priceper), 
                                                  rpp_median_priceper = median(priceper)),
                                                by = .(lsoa_code, year)][order(lsoa_code, year)]

property_prices_lsoa_year <- property_prices_lsoa_year %>%
  complete(lsoa_code = c(unique(property_prices$lsoa_code),"E01002907"), year, fill = list(rpp_n_sales = 0)) %>%
  mutate(rpp_n_sales = ifelse(lsoa_code == "E01002907", NA, rpp_n_sales))

save(property_prices_lsoa_year, file = "./data/derived/property_prices/property_prices_lsoa_year.RData")