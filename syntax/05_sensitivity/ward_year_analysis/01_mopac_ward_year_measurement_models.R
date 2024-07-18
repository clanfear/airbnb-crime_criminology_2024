# This script calculates collective efficacy at the ward level using different
# measurement models: (1) A two-stage process with an individual-level CFA and a 
# two-level HLM; (2) a three-level linear HLM. These are both done with and without
# imputation. Results are basically the same no matter what you do.

library(tidyverse)
library(lavaan)
library(miceRanger, pos = 999)
library(MASS, pos = 999)
library(Matrix, pos = 999)
library(lme4, pos = 999)
library(arm, pos = 999)
library(merTools, pos = 999)
library(foreach, pos = 999)
library(doParallel, pos = 999)
library(furrr)
load("./data/derived/mopac/pas_15_18_wide.RData")
load("./data/derived/mopac/pas_15_18_long.RData")
source("./syntax/project_functions.R")
plan(multisession, workers = 4)
run_scratch <- FALSE # Flip to true to rerun everything from scratch
####
# CRUDE CE (MEAN)
# As a baseline, calculate neighb scores as grand mean of indicator scores.

ce_crude_ward_year <- pas_15_18_long %>%
  filter(str_detect(measure, "coh_|inf_")) %>%
  group_by(ward_code, year, id) %>%
  summarize(ce = mean(value, na.rm=TRUE)) %>%
  summarize(ce = mean(ce, na.rm=TRUE), .groups = "drop")

####
# CE FROM 3 LEVEL LINEAR MODEL (SRE 1997)
# 3 level, linear model based, using interaction term of year/ward
if(file.exists("./data/derived/mopac/ce_ward_year_3lvl.RData") & !run_scratch){
  load("./data/derived/mopac/ce_ward_year_3lvl.RData")
} else {
lmer_ward_year_out_3lvl <- lmer(value ~ measure + race + tenure_num + age_num + employment + homeowner + gender + victim_year  + (1|year_ward/id), REML = FALSE, data = pas_15_18_long %>%
                             mutate(year_ward = paste0(year, "_", ward_code)) %>%
                             filter(!str_detect(measure, "^per")))
summary(lmer_ward_year_out_3lvl)
lme_reliability_3lvl(lmer_ward_year_out_3lvl)

ce_ward_year_3lvl <- ranef(lmer_ward_year_out_3lvl, condVar = FALSE)$year_ward |> 
  rownames_to_column("year_ward") |> tibble() |> separate(year_ward, into = c("year", "ward_code")) |> 
  transmute(year = as.numeric(year), ward_code, ce = standardize(`(Intercept)`)) |>
  arrange(ward_code, year)

save(ce_ward_year_3lvl, file = "./data/derived/mopac/ce_ward_year_3lvl.RData")
}

# Two ways to do this: Separate borough and date terms or just an interaction
# They produce identical results but the single interaction term is simpler to use
# 2 Level, factor-based, using interaction term of year/ward

####
# CE FROM 3 LEVEL MODEL WITH IMPUTATION
# The MOPAC data have fairly little missingness, but missings take a heavy toll
# when trying to calculate neighborhood-level estimates. So best to impute then
# run the models. Here I use miceRanger which does multiple imputation by 
# chained equations using random forests. This is a lot more time and memory
# efficient than standard MICE.

# 10 imputed datasets should be incredibly precise given the small missingness.
# 5 iterations looks like plenty in the accuracy plots
# 100 trees seems accurate without being punishingly slow

pas_15_18_wide %>%
  select(year_ward, id, race, tenure_num, age_num, employment, homeowner, gender, victim_year, matches("^(coh|inf)")) %>%
  list_missing()

if(file.exists("./data/derived/mopac/pas_imputed.RData") & !run_scratch){
  load("./data/derived/mopac/pas_imputed.RData")
} else{
  pas_imputed <- pas_15_18_wide %>%
    select(year_ward, id, race, tenure_num, age_num, employment, homeowner, gender, victim_year, matches("^(coh|inf)")) %>%
    mutate(across(everything(), ~ factor(., ordered = FALSE))) %>%
    miceRanger(data = ., m = 10, maxiter = 5, num.trees = 100, valueSelector = "meanMatch",
               vars = c("race", "tenure_num", "age_num", "employment", "homeowner", "gender",
                        "victim_year", "coh_trust", "coh_courtesy", "coh_take_pride",
                        "coh_get_help", "coh_get_along", "inf_have_control", "inf_call_police" ,
                        "inf_sanction"))
  save(pas_imputed, file = "./data/derived/mopac/pas_imputed.RData")
}

plotDistributions(pas_imputed)
plotVarConvergence(pas_imputed)
plotModelError(pas_imputed)

# Need to pivot every df long then run the model over every imputed dataset
# future might buy some speed here.
if(file.exists("./data/derived/mopac/ce_ward_year_3lvl_imputed.RData") & !run_scratch){
  load("./data/derived/mopac/ce_ward_year_3lvl_imputed.RData")
} else {
imputed_lmer <- completeData(pas_imputed) %>%
  map(., ~ mutate(.x, across(matches("^(coh|inf)"), ~ as.numeric(.))) |> pivot_longer(matches("^(coh|inf)"), names_to = "measure", values_to = "value")) %>%
  future_map(~ lmer(value ~ measure + race + tenure_num + age_num + employment + homeowner + gender + victim_year  + (1|year_ward/id), data = .x, REML = FALSE))

# Calc reliability as the average over all models
message(paste0("Mean reliability across ", length(imputed_lmer), " imputed sets: ", round(mean(future_map_dbl(imputed_lmer, ~lme_reliability_3lvl(.x))), 3)))

# Quick function to take a list of lmer models and pull random effects then
# take the mean for each year_ward
extract_mean_re <- function(x){
  future_map_dfr(x, ~tibble::rownames_to_column(ranef(.x)$year_ward, "year_ward")) |>
    group_by(year_ward) |>
    summarize(ce = mean(`(Intercept)`)) |>
    separate(year_ward, into = c("year", "ward_code")) |>
    mutate(year = as.numeric(year))
}
ce_ward_year_3lvl_imputed <- extract_mean_re(imputed_lmer)

save(ce_ward_year_3lvl_imputed, file = "./data/derived/mopac/ce_ward_year_3lvl_imputed.RData")
}

####
# INDIVIDUAL CE FROM FACTOR MODEL (Matsueda & Drakulich 2017)
if (file.exists("./data/derived/mopac/ce_ward_year_2lvl.RData") & !run_scratch){
  load("./data/derived/mopac/ce_ward_year_2lvl.RData")
} else {
fac_form <- "
ce =~ coh_courtesy + coh_trust  + coh_take_pride + coh_get_help + coh_get_along + inf_have_control + inf_call_police + inf_sanction
"
fac_form_separate <- "
coh =~ coh_courtesy + coh_trust  + coh_take_pride + coh_get_help + coh_get_along
inf =~ inf_have_control + inf_call_police + inf_sanction
coh ~~ inf
"
# FIML, NO IMPUTATION

pas_cfa <- cfa(fac_form, data = pas_15_18_wide, estimator = "PML", missing = "pairwise")
pas_cfa_separate <- cfa(fac_form_separate, data = pas_15_18_wide, estimator = "PML", missing = "pairwise")
pas_fs  <- cbind(lavPredict(pas_cfa, method = "EBM"), lavPredict(pas_cfa_separate, method = "EBM"), pas_15_18_wide)

lmer_ward_year_out_2lvl <- lmer(ce ~ race + tenure_num + age_num + employment + homeowner + gender + victim_year  + (1|year_ward), data = pas_fs, REML = FALSE)
summary(lmer_ward_year_out_2lvl)
lme_reliability_2lvl(lmer_ward_year_out_2lvl)

lmer_ward_year_out_2lvl_inf <- lmer(inf ~ race + tenure_num + age_num + employment + homeowner + gender + victim_year  + (1|year_ward), data = pas_fs, REML = FALSE)
summary(lmer_ward_year_out_2lvl_inf)
lme_reliability_2lvl(lmer_ward_year_out_2lvl_inf)

lmer_ward_year_out_2lvl_coh <- lmer(coh ~ race + tenure_num + age_num + employment + homeowner + gender + victim_year  + (1|year_ward), data = pas_fs, REML = FALSE)
summary(lmer_ward_year_out_2lvl_coh)
lme_reliability_2lvl(lmer_ward_year_out_2lvl_coh)

ce_ward_year_2lvl <- ranef(lmer_ward_year_out_2lvl)$year_ward |>
  tibble::rownames_to_column("year_ward") |>
  separate(year_ward, into = c("year", "ward_code")) |> 
  transmute(year = as.numeric(year), ward_code, ce = standardize(`(Intercept)`)) |>
  arrange(ward_code, year)

inf_ward_year_2lvl <- ranef(lmer_ward_year_out_2lvl_inf)$year_ward |>
  tibble::rownames_to_column("year_ward") |>
  separate(year_ward, into = c("year", "ward_code")) |> 
  transmute(year = as.numeric(year), ward_code, inf = standardize(`(Intercept)`)) |>
  arrange(ward_code, year)

coh_ward_year_2lvl <- ranef(lmer_ward_year_out_2lvl_coh)$year_ward |>
  tibble::rownames_to_column("year_ward") |>
  separate(year_ward, into = c("year", "ward_code")) |> 
  transmute(year = as.numeric(year), ward_code, coh = standardize(`(Intercept)`)) |>
  arrange(ward_code, year)

ce_ward_year_2lvl <- ce_ward_year_2lvl %>%
  full_join(inf_ward_year_2lvl) %>%
  full_join(coh_ward_year_2lvl)

ce_ward_year_2lvl %>% select(ce, coh, inf) %>% cor()

save(ce_ward_year_2lvl, file = "./data/derived/mopac/ce_ward_year_2lvl.RData")
}
# IMPUTATION
if (file.exists("./data/derived/mopac/ce_ward_year_2lvl.RData") & !run_scratch){
  load("./data/output/pas_cfa_imputed_loadings.RData")
} else {
pas_cfa_imputed <- completeData(pas_imputed) %>%
  map(., ~ cfa(fac_form, data = mutate(.x, across(matches("^(coh|inf)"), ~ ordered(.)))))

pas_cfa_imputed_loadings <- pas_cfa_imputed %>% 
  map_dfr(~broom::tidy(.)) %>%
  filter(op == "=~") %>%
  mutate(term = str_remove(term, "ce =~ ")) %>%
  group_by(term) %>%
  summarize(estimate = mean(estimate))
save(pas_cfa_imputed_loadings, file = "./data/output/pas_cfa_imputed_loadings.RData")
}

pas_cfa_imputed_separate <- completeData(pas_imputed) %>%
  map(., ~ cfa(fac_form_separate, data = mutate(.x, across(matches("^(coh|inf)"), ~ ordered(.)))))

pas_cfa_fit <- map_dfr(pas_cfa_imputed, fitmeasures) %>%
  select(chisq, pvalue, chisq.scaled, pvalue.scaled, cfi, tli, rmsea, rmsea.scaled, srmr)

if(file.exists("./data/derived/mopac/pas_imputed.RData") & !run_scratch){
  load("./data/derived/mopac/pas_fs_imputed.RData")
} else {
  pas_fs_imputed <- map2(.x = pas_cfa_imputed, .y = completeData(pas_imputed), ~cbind(lavPredict(.x, method = "EBM", append.data = FALSE), .y))
  save(pas_fs_imputed, file = "./data/derived/mopac/pas_fs_imputed.RData")
}

if(file.exists("./data/derived/mopac/ce_ward_year_2lvl_imputed.RData") & !run_scratch){
  load("./data/derived/mopac/ce_ward_year_2lvl_imputed.RData")
} else {
    imputed_2lvl_lmer <- pas_fs_imputed %>%
    future_map(~ lmer(ce ~ race + tenure_num + age_num + employment + homeowner + gender + victim_year  + (1|year_ward), data = .x, REML = FALSE))
  # Calc reliability as the average over all models
  message(paste0("Mean reliability across ", length(imputed_2lvl_lmer), " imputed sets: ", round(mean(future_map_dbl(imputed_2lvl_lmer, ~lme_reliability_2lvl(.x))), 3)))
  ce_ward_year_2lvl_imputed <- extract_mean_re(imputed_2lvl_lmer)
  save(ce_ward_year_2lvl_imputed, file = "./data/derived/mopac/ce_ward_year_2lvl_imputed.RData")
}

imputed_2lvl_lmer_ward <- pas_fs_imputed %>%
  future_map(~ lmer(ce ~ race + tenure_num + age_num + employment + homeowner + gender + victim_year  + (1|ward_code), data = separate(.x, year_ward, into = c("year", "ward_code")), REML = FALSE))
# Calc reliability as the average over all models
message(paste0("Mean reliability across ", length(imputed_2lvl_lmer_ward), " imputed sets: ", round(mean(future_map_dbl(imputed_2lvl_lmer_ward, ~lme_reliability_2lvl(.x))), 3)))

ce_ward_year_2lvl_imputed_ward <- future_map_dfr(imputed_2lvl_lmer_ward, ~tibble::rownames_to_column(ranef(.x)$ward_code, "ward_code")) |>
  group_by(ward_code) |>
  summarize(ce = mean(`(Intercept)`))

save(ce_ward_year_2lvl_imputed_ward, file = "./data/derived/mopac/ce_ward_year_2lvl_imputed_ward.RData")



# Compare methods
ce_ward_year_all_methods <- ce_ward_year_2lvl %>%
  rename(ce_2lvl = ce) %>%
  full_join(ce_ward_year_3lvl %>% rename(ce_3lvl = ce)) %>% 
  full_join(ce_crude_ward_year %>% rename(ce_crude = ce)) %>% 
  full_join(ce_ward_year_3lvl_imputed %>% rename(ce_imputed_3lvl = ce)) %>%
  full_join(ce_ward_year_2lvl_imputed %>% rename(ce_imputed_2lvl = ce)) %>%
  mutate(across(matches("^ce"), ~standardize(.)))

save(ce_ward_year_all_methods, file = "./data/derived/mopac/ce_ward_year_all_methods.RData")

ce_ward_year_all_methods %>%
  select(matches("^ce")) %>% 
  cor(use = "pairwise.complete")

# Yep, they're all basically the same