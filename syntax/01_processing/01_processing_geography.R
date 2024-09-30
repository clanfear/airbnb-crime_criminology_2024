# This processes shapefiles to get all necessary boundaries: LSOAs, Wards,
# water features (for plots), and boroughs. It also gets demographic data.
# Using EPSG:27700 OSGB36 / British National Grid for coordinate reference

library(tidyverse)
library(sf)
library(janitor)
library(spdep)

source("./syntax/project_functions.R")

# 2011 LSOA to borough crosswalk
london_lsoa_to_borough <- st_read("./data/raw/statistical-gis-boundaries-london/ESRI/LSOA_2011_London_gen_MHW.shp") %>% 
  st_drop_geometry() %>%
  distinct(lsoa_code = LSOA11CD, borough_code = LAD11CD)

# LSOA boundaries
london_lsoa    <- st_read("./data/raw/Lower_Layer_Super_Output_Areas__December_2011__Boundaries_Full_Extent__BFE__EW_V3-shp/Lower_Layer_Super_Output_Areas__December_2011__Boundaries_Full_Extent__BFE__EW_V3.shp") %>% 
  st_make_valid() %>%
  clean_names() %>%
  rename(lsoa_code = lsoa11cd) %>%
  inner_join(london_lsoa_to_borough) %>%
  st_transform(27700) %>%
  mutate(city_of_london = borough_code == "E09000001")

# Ward boundaries
london_ward    <- st_read("./data/raw/London-wards-2018_ESRI/London_Ward.shp") %>% 
  st_make_valid() %>%
  clean_names() %>%
  rename(ward_code = gss_code,
         borough_code = lagsscode) %>%
  mutate(city_of_london = borough_code == "E09000001") %>%
  st_transform(27700)

# 2001 LSOAs were used for a small number of MOPAC surveys, so we need these too
london_lsoa_2001 <- st_read("./data/raw/Lower_Layer_Super_Output_Areas__December_2001__EW_BFE-shp/Lower_Layer_Super_Output_Areas__December_2001__EW_BFE.shp") %>% 
  st_make_valid() %>%
  clean_names() %>%
  rename(lsoa_code_2001 = lsoa01cd) %>%
  st_transform(27700) %>%
  st_filter(london_ward %>% st_union())

# Boroughs
london_borough <- st_read("./data/raw/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp") %>% 
  st_make_valid() %>%
  clean_names() %>%
  mutate(borough_code = gss_code) %>%
  st_transform(27700)

# Generate neighbor matrices
london_lsoa_neighbors <- london_lsoa |> get_neighbors("lsoa_code")
london_ward_neighbors <- london_ward |> get_neighbors("ward_code")
london_borough_neighbors <- london_borough |> get_neighbors("borough_code")

# Demographic data, including an LSOA area measure
london_lsoa_pop <- st_read("./data/raw/statistical-gis-boundaries-london/ESRI/LSOA_2011_London_gen_MHW.shp") %>% 
  st_drop_geometry() %>% 
  clean_names() %>%
  transmute(lsoa_code  = lsoa11cd, 
            population = usualres,
            density    = popden,
            households = hholds,
            lsoa_area = units::set_units((population / density)/100, "km^2"))
  
save(london_lsoa_pop, file = "./data/derived/london_lsoa_pop.RData")

# Water features for plots; surface (river / lake) and tidal are separate
london_water   <- 
  st_read("./data/raw/OS VectorMap District (ESRI Shape File) TQ/data/TQ_SurfaceWater_Area.shp") %>% 
  st_filter(london_ward) %>% 
  clean_names() %>%
  st_combine() %>%
  st_union() %>%
  st_make_valid() %>%
  st_union(st_read("./data/raw/OS VectorMap District (ESRI Shape File) TQ/data/TQ_TidalWater.shp") %>% 
             st_filter(london_ward) %>% 
             clean_names() %>%
             st_combine() %>%
             st_union() %>%
             st_make_valid()) %>%
  st_union() %>%
  st_make_valid() %>%
  st_transform(27700)

# Write everything to disk
save(london_lsoa, file    = "./data/derived/shape/london_lsoa.RData")
save(london_lsoa_2001, file    = "./data/derived/shape/london_lsoa_2001.RData")
save(london_ward, file    = "./data/derived/shape/london_ward.RData")
save(london_borough, file = "./data/derived/shape/london_borough.RData")
save(london_water, file   = "./data/derived/shape/london_water.RData")
save(london_lsoa_neighbors, file = "./data/derived/neighbors/london_lsoa_neighbors.RData")
save(london_ward_neighbors, file = "./data/derived/neighbors/london_ward_neighbors.RData")
save(london_borough_neighbors, file = "./data/derived/neighbors/london_borough_neighbors.RData")

