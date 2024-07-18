# In theory this script will reproduce the entire project from scratch if you
# have all required data in the required paths as seen in the readme. In general
# you would not want to do this because it would take a very long time. But, you
# can use this script as a basis to get things working.

# REQUIRED PACKAGES

required_packages <- list.files("./syntax/", full.names = TRUE, recursive = TRUE) |>
  lapply(readLines, warn = FALSE) |>
  unlist() |>
  stringr::str_subset("(library\\(.*\\))|([A-Za-z]+::)") |>
  stringr::str_extract("(library\\(.*\\))|([A-Za-z]+::)") |>
  stringr::str_remove_all("(library\\()|(\\))|(::)") |>
  stringr::str_remove_all(",.*$") |>
  c("officedown", "ggspatial") |> 
  unique() |>
  sort()
required_packages

# Uncomment and run these lines to install all necessary packages

# install.packages(required_packages)
# Also need a slightly modified version of DPM package I made to address a bug
# remotes::install_github("clanfear/dpm")

library(tidyverse)

# Making data directories in case they didn't come down with repo
make_directory <- function(x){
  if (!dir.exists(x)) {dir.create(x, recursive = TRUE)}
}
c(paste0("data/", c("raw", "output", "derived", "analytical")),
  paste0("data/derived/", c("airbnb", "crime", "mopac", "neighbors", "property_prices", "shape"))
  ) |> walk(make_directory)

# This will walk through all the files using source. I wouldn't recommend using
# this to reproduce the project but it should normally work.
core_syntax_files <- list.files("./syntax/", full.names = TRUE, recursive = TRUE, pattern = "\\.R$") |>
  str_subset("01_processing|02_models|03_descriptive|04_figures")
walk(core_syntax_files, source)
