# Sensitivity test reducing resolution of ward analyses to half-years

library(tidyverse)
library(lavaan)
library(sf)
library(miceRanger, pos = 999)
library(MASS, pos = 999)
library(Matrix, pos = 999)
library(lme4, pos = 999)
library(arm, pos = 999)
library(merTools, pos = 999)
library(furrr)
load("./data/derived/mopac/pas_15_18_wide.RData")
load("./data/derived/crime/ward_crime_weighted.RData")
load("./data/derived/airbnb/airdna_ward_disagg.RData")
load("./data/derived/shape/london_ward.RData")
source("./syntax/project_functions.R")

if(file.exists("./data/derived/mopac/pas_imputed.RData")){
  load("./data/derived/mopac/pas_imputed.RData")
} else{
  pas_imputed <- pas_15_18_wide |>
    mutate(half_ward = str_c(year_half, "_", ward_code)) |>
    select(half_ward, id, race, tenure_num, age_num, employment, homeowner, gender, victim_year, see_patrols, matches("^(coh|inf|perdis)")) %>%
    mutate(across(everything(), ~ factor(., ordered = FALSE))) |>
    miceRanger(data = _, m = 10, maxiter = 5, num.trees = 100, valueSelector = "meanMatch",
               vars = c("race", "tenure_num", "age_num", "employment", "homeowner", "gender",
                        "victim_year", "coh_trust", "coh_courtesy", "coh_take_pride",
                        "coh_get_help", "coh_get_along", "inf_have_control", "inf_call_police" ,
                        "inf_sanction", "see_patrols", 
                        "perdis_noise", "perdis_teens", "perdis_litter", "perdis_vandalism", "perdis_drunk"))
}
# Calculate collective efficacy
cd_fac_form <- "
ce =~ coh_courtesy + coh_trust  + coh_take_pride + coh_get_help + coh_get_along + inf_have_control + inf_call_police + inf_sanction
"

pas_cfa_imputed <- completeData(pas_imputed) |>
  map(~ cfa(cd_fac_form, data = mutate(.x, across(matches("^(coh|inf)"), ~ ordered(.)))))

# Pull loadings for appendix descriptives
pas_cfa_imputed_loadings <- pas_cfa_imputed %>% 
  map_dfr(~broom::tidy(.)) %>%
  filter(op == "=~") %>%
  mutate(term = str_remove(term, "ce =~ ")) %>%
  group_by(term) %>%
  summarize(estimate = mean(estimate))
save(pas_cfa_imputed_loadings, file = "./data/output/pas_cfa_imputed_loadings.RData")

pas_cfa_fit <- map_dfr(pas_cfa_imputed, fitmeasures) |>
  select(chisq, pvalue, chisq.scaled, pvalue.scaled, cfi, tli, rmsea, rmsea.scaled, srmr)

pas_fs_imputed <- map2(.x = pas_cfa_imputed, .y = completeData(pas_imputed), ~cbind(lavPredict(.x, method = "EBM", append.data = FALSE), .y))

imputed_2lvl_lmer <- pas_fs_imputed |>
    future_map(~ lmer(ce ~ race + tenure_num + age_num + employment + homeowner + gender + victim_year  + (1|half_ward), data = .x, REML = FALSE))
  # Calc reliability as the average over all models
message(paste0("Mean reliability across ", length(imputed_2lvl_lmer), " imputed sets: ", round(mean(future_map_dbl(imputed_2lvl_lmer, ~lme_reliability_2lvl(.x))), 3)))


extract_mean_re <- function(x){
  future_map_dfr(x, ~tibble::rownames_to_column(ranef(.x)$half_ward, "half_ward")) |>
    group_by(half_ward) |>
    summarize(ce = mean(`(Intercept)`)) |>
    separate(half_ward, into = c("year_half", "ward_code"), sep = "_") |>
    mutate(year_half = as.character(year_half))
}

ce_ward_half_2lvl_imputed <- extract_mean_re(imputed_2lvl_lmer)

save(ce_ward_half_2lvl_imputed, file = "./data/derived/mopac/ce_ward_half_2lvl_imputed.RData")


# Disorder for sensitivity test
dis_fac_form <- "
dis =~ perdis_noise + perdis_teens  + perdis_litter + perdis_vandalism + perdis_drunk
"

pas_cfa_dis_imputed <- completeData(pas_imputed) |>
  map(~ cfa(dis_fac_form, data = mutate(.x, across(matches("^perdis"), ~ ordered(.)))))

pas_cfa_dis_fit <- map_dfr(pas_cfa_dis_imputed, fitmeasures) |>
  select(chisq, pvalue, chisq.scaled, pvalue.scaled, cfi, tli, rmsea, rmsea.scaled, srmr)

pas_fs_dis_imputed <- map2(.x = pas_cfa_dis_imputed, .y = completeData(pas_imputed), ~cbind(lavPredict(.x, method = "EBM", append.data = FALSE), .y))
imputed_2lvl_dis_lmer <- pas_fs_dis_imputed |>
  future_map(~ lmer(dis ~ race + tenure_num + age_num + employment + homeowner + gender + victim_year  + (1|half_ward), data = .x, REML = FALSE))
# Calc reliability as the average over all models: 0.625 here
message(paste0("Mean reliability across ", length(imputed_2lvl_dis_lmer), " imputed sets: ", round(mean(future_map_dbl(imputed_2lvl_dis_lmer, ~lme_reliability_2lvl(.x))), 3)))

dis_ward_half_2lvl_imputed <- extract_mean_re(imputed_2lvl_dis_lmer) |> rename(dis = ce)

save(dis_ward_half_2lvl_imputed, file = "./data/derived/mopac/dis_ward_half_2lvl_imputed.RData")
