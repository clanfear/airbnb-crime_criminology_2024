# This script generates all figures for main text and appendix as PDF for final 
# paper submission. Doing it all in one script because they're pretty redundant.

library(tidyverse)
library(sf)
library(ggtext)
library(patchwork)
library(ggspatial)
library(ggh4x)
library(rmarkdown)
source("./syntax/project_functions.R")

load("./data/output/abnb_density_data.RData")
load("./data/derived/shape/london_lsoa.RData")
load("./data/derived/shape/london_water.RData")

save_airbnb_fig <- function(plot, fig_name, width = 6.5, height = 6, loc = "figures"){
  filename <- glue::glue("./docs/{loc}/{fig_name}.pdf")
  message(str_c("Saving ", filename))
  ggsave(filename = filename, plot = plot, device = "pdf", width = width, height = height, units = "in")
}

# FIG 1: Airbnb density map

abnb_density_plot <- abnb_density_data %>%
  mutate(abnb_density = ifelse(abnb_density == 0, NA, abnb_density)) %>%
  ggplot(aes(fill = abnb_density)) +
  geom_sf(color = NA) + 
  geom_sf(data = london_water, color = NA, fill = "skyblue", inherit.aes = FALSE) +
  theme_void() + 
  annotation_scale(location = "bl", text_family = "serif",) +
  annotation_north_arrow(which_north = "grid",
                         location    = "tl",
                         style       = north_arrow_orienteering(text_family = "serif"),
                         height      = unit(1, "cm"),
                         width       = unit(1, "cm")) +
  scale_fill_viridis_c(option = "inferno", labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  labs(fill = "Properties<br>per KM^2") + 
  theme(legend.title  = element_markdown(size = 8),
        legend.position = "right",
        text = element_text(family = "serif"))
abnb_density_plot
save_airbnb_fig(abnb_density_plot, "fig-1-map", height = 4)

# FIG 2 and 3 are generated via RMarkdown files

render("./syntax/04_figures/fig-2-theory-dag.Rmd", output_file = "fig-2-theory-dag.pdf", output_dir = "./docs/figures/")
render("./syntax/04_figures/fig-3-model-dag.Rmd", output_file = "fig-3-model-dag.pdf", output_dir = "./docs/figures/")

# FIG 4: ML-SEM LSOA quarter results
load("./data/output/dpm_quarter_fit.RData")
dpm_quarter_fit_plot <- dpm_coef_plot(dpm_quarter_fit, hide_lag = FALSE)
dpm_quarter_fit_plot
save_airbnb_fig(dpm_quarter_fit_plot, "fig-4-ml-sem")

# FIG 5: ML-SEM LSOA quarter disaggregated property type results
load("./data/output/dpm_quarter_diffprops_fit.RData")
dpm_quarter_diffprops_fit_plot <- dpm_coef_plot(dpm_quarter_diffprops_fit, hide_lag = TRUE, con_only = TRUE)
dpm_quarter_diffprops_fit_plot
save_airbnb_fig(dpm_quarter_diffprops_fit_plot, "fig-5-ml-sem-diffprops", height = 3)

# FIG 6: ML-SEM ward half-year results
load("./data/output/dpm_half_fit.RData")
dpm_half_fit_plot <- dpm_coef_plot(dpm_half_fit)
dpm_half_fit_plot
save_airbnb_fig(dpm_quarter_diffprops_fit_plot, "fig-6-ml-sem-half-yearly", width = 6, height = 6)

# FIG 7: ML-SEM ward half-year collective efficacy / airbnb reciprocality results
load("./data/output/dpm_half_ce_abnb_fit.RData")
dv_ce <- ggplot(dpm_half_ce_abnb_fit %>% 
                  filter(dv == "Collective Efficacy" & !str_detect(term, "Collective Efficacy")) |>
                  mutate(spec = fct_relevel(spec, "Both", "Lag", "Con")), aes(x = estimate, y = spec)) + 
  ylab("*Specification*") + xlab(NULL) +
  geom_point() + 
  facet_grid( ~ term, switch = "y") + 
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), size = 0.2, height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_x_continuous(breaks = c(-.1, 0, .1), labels = no_lead_zero) +
  theme_minimal() + 
  ggtitle("Outcome:\nCollective Efficacy") +
  theme(text = element_text(family = "serif"),
        panel.grid.major.y  = element_blank(),
        panel.grid.minor.x  = element_blank(),
        panel.spacing.y =unit(0,"lines"),
        strip.placement = "outside",
        strip.text.y = element_text(face = "bold"),
        axis.text.y = element_text(face = "italic"),
        axis.title.y = ggtext::element_markdown())

dv_abnb <- ggplot(dpm_half_ce_abnb_fit %>% 
                    filter(dv == "Active Rentals" & !str_detect(term, "Active Rentals"))|>
                    mutate(spec = fct_relevel(spec, "Both", "Lag", "Con")), aes(x = estimate, y = spec)) + 
  ylab(NULL) + xlab(NULL) +
  geom_point() + 
  facet_grid( ~ term, switch = "y") + 
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), size = 0.2, height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_x_continuous(breaks = c(-.01, 0, .01), labels = no_lead_zero) +
  theme_minimal() +
  ggtitle("Outcome:\nLog of Active Airbnb Rentals") +
  theme(text = element_text(family = "serif"),
        panel.grid.major.y  = element_blank(),
        panel.grid.minor.x  = element_blank(),
        panel.spacing.y =unit(0,"lines"),
        strip.placement = "outside",
        strip.text.y = element_text(face = "bold"),
        axis.text.y = element_blank(),
        axis.title.y = ggtext::element_markdown(),
        strip.clip = "off")
ce_abnb_combined_plot <- dv_ce + dv_abnb
ce_abnb_combined_plot
save_airbnb_fig(ce_abnb_combined_plot, "fig-7-ml-sem-half-yearly-ce-abnb", width = 6, height = 3)

# FIG 8: ML-SEM ward half-year collective efficacy results
load("./data/output/dpm_half_ce_fit.RData")
dpm_half_ce_fit_plot <- dpm_half_ce_fit %>%
  mutate(term = str_replace(term, "std_collective_efficacy", "Collective\nEfficacy")) %>%
  dpm_coef_plot()
dpm_half_ce_fit_plot
save_airbnb_fig(dpm_half_ce_fit_plot, "fig-8-ml-sem-half-yearly-ce", width = 6)

##################
# APPENDIX FIGURES
##################

# FIG A1: Population Density

load("./data/output/abnb_density_data.RData")
load("./data/derived/shape/london_water.RData")
pop_density_plot <- abnb_density_data %>%
  mutate(popden = ifelse(popden == 0, NA, popden)) %>%
  mutate(abnb_density = ifelse(abnb_density == 0, NA, abnb_density)) %>%
  ggplot(aes(fill = popden)) +
  geom_sf(color = NA) + 
  geom_sf(data = london_water, color = NA, fill = "skyblue", inherit.aes = FALSE) +
  theme_void() + 
  annotation_scale(location = "bl", text_family = "serif",) +
  annotation_north_arrow(which_north = "grid",
                         location    = "tl",
                         style       = north_arrow_orienteering(text_family = "serif"),
                         height      = unit(1, "cm"),
                         width       = unit(1, "cm")) +
  scale_fill_viridis_c(option = "inferno", labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  labs(fill = "Residents<br>per KM^2") + 
  theme(legend.title  = element_markdown(size = 8),
        legend.position = "right",
        text = element_text(family = "serif"))
pop_density_plot
save_airbnb_fig(pop_density_plot, "fig-a1-pop-density", height = 4, loc = "figure-appendix")


# FIG A2: Unstandardized Estimates

load("./data/output/dpm_quarter_fit_unstd.RData")
dv_levels <- c("Robbery", "Burglary", "Theft", "ASB", "Violence", "Harm")

plot_data <- dpm_quarter_fit_unstd %>%
  select(dv, spec, term, estimate, std.error, conf.low, conf.high) %>%
  mutate(term = ifelse(str_detect(term, "dp|dlg") & str_detect(term, "t - 1"), "crime (t - 1)", term),
         across(c(dv, term), ~ str_to_title(str_replace_all(str_remove_all(., "(log_)|(std_)|(dlg_|dp_|abnb_)|(_aw$)?"), "_", " "))),
         spec = fct_rev(factor(str_to_title(spec), levels = c("Con", "Both", "Lag"))),
         dv = fct_relevel(str_replace_all(dv, c("Violence Harm" = "Harm", "Asb" = "ASB")), dv_levels))
plot_data <- plot_data %>% filter(term != "Crime (T - 1)")

dpm_quarter_fit_unstd_plot <- ggplot(plot_data, aes(x = estimate, y = spec, group = dv)) + 
  geom_rect(data = filter(plot_data, dv %in% dv_levels[c(2,4,6)] & spec == "Both"), fill = "black", xmin = -Inf, xmax = Inf,
            ymin = -Inf, ymax = Inf, alpha = 0.1) +
  ylab("**Crime type** and *Specification*") + xlab(NULL) +
  geom_point() + 
  facet_grid(dv ~ term, switch = "y", scales = "free_y") + 
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4, size = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_x_continuous(label = no_lead_zero) +
  theme_minimal() + 
  theme(text = element_text(family = "serif"),
        panel.grid.major.y  = element_blank(),
        panel.grid.minor.x  = element_blank(),
        panel.spacing.y =unit(0,"lines"),
        panel.spacing.x =unit(1,"lines"),
        strip.placement = "outside",
        strip.text.y = element_text(face = "bold"),
        axis.text.y = element_text(face = "italic"),
        axis.title.y = ggtext::element_markdown())
dpm_quarter_fit_unstd_plot
save_airbnb_fig(dpm_quarter_fit_unstd_plot, "fig-a2-dpm-unstd", loc = "figure-appendix")

# FIG A3:  Unstandardized estimates with different prop types
load("./data/output/dpm_quarter_diffprops_fit_unstd.RData")

plot_data <- dpm_quarter_diffprops_fit_unstd %>%
  filter(spec == "con") |>
  select(dv, spec, term, estimate, std.error, conf.low, conf.high) %>%
  mutate(term = ifelse(str_detect(term, "dp|dlg") & str_detect(term, "t - 1"), "crime (t - 1)", term),
         across(c(dv, term), ~ str_to_title(str_replace_all(str_remove_all(., "(log_)|(std_)|(dlg_|dp_|abnb_)|(_aw$)?"), "_", " "))),
         spec = fct_rev(factor(str_to_title(spec), levels = c("Con", "Both", "Lag"))),
         dv = fct_relevel(str_replace_all(dv, c("Violence Harm" = "Harm", "Asb" = "ASB")), dv_levels)) %>%  
  filter(term != "Crime (T - 1)") |>
  mutate(dv = fct_rev(dv)) %>%
  mutate(graybar = ifelse(dv %in% c("Burglary", "ASB", "Harm"), "black", "white"))

dpm_quarter_diffprops_fit_unstd_plot <- ggplot(plot_data, aes(x = estimate, y = dv)) + 
  geom_tile(aes(alpha = graybar), 
            width = Inf, height = 1, fill = "black") +
  scale_alpha_manual(values = c("black" = 0.3, "white" = 0)) +
  ylab("**Crime type**") + xlab(NULL) +
  geom_point() + 
  facet_grid( ~ term) + 
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, size = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  theme_minimal() + 
  coord_cartesian(xlim = c(-0.25, 0.3)) +
  scale_x_continuous(label = no_lead_zero) +
  theme(text = element_text(family = "serif"),
        panel.grid.major.y  = element_blank(),
        panel.grid.minor.x  = element_blank(),
        panel.spacing.y = unit(0,"lines"),
        strip.placement = "outside",
        strip.text.y = element_text(face = "bold"),
        # axis.text.y = element_text(face = "bold"),
        axis.title.y = ggtext::element_markdown(),
        legend.position = "none")
dpm_quarter_diffprops_fit_unstd_plot
save_airbnb_fig(dpm_quarter_diffprops_fit_unstd_plot, "fig-a3-dpm-unstd", loc = "figure-appendix")

# Property Usage

load("./data/output/dpm_alt_specs/dpm_quarter_usage_fit.RData")

dpm_quarter_usage_fit_plot <- dpm_coef_plot(dpm_quarter_usage_fit)
dpm_quarter_usage_fit_plot
save_airbnb_fig(dpm_quarter_usage_fit_plot, "fig-a4-dpm-usage", loc = "figure-appendix")

# Airbnb Concentration
load("./data/output/dpm_alt_specs/dpm_quarter_nni_fit.RData")

dpm_quarter_nni_fit_plot <- dpm_quarter_nni_fit |>
  select(dv, spec, term, estimate, std.error, conf.low, conf.high) %>%
  mutate(term = ifelse(str_detect(term, "dp|dlg") & str_detect(term, "t - 1"), "crime (t - 1)", term),
         across(c(dv, term), ~ str_to_title(str_replace_all(str_remove_all(., "(log_)|(std_)|(dlg_|dp_|abnb_)|(_aw$)?"), "_", " "))),
         term = ifelse(str_detect(term, "Nni"), str_replace(term, "Nni", "NNI"), term),
         spec = fct_rev(factor(str_to_title(spec), levels = c("Con", "Lag", "Both")))) |>
  ggplot(aes(x = estimate, y = spec, group = dv)) + 
    geom_rect(data = filter(plot_data, dv %in% dv_levels[c(2,4,6)] & spec == "Both"), fill = "black", xmin = -Inf, xmax = Inf,
              ymin = -Inf, ymax = Inf, alpha = 0.1) +
    ylab("**Crime type** and *Specification*") + xlab(NULL) +
    geom_point() + 
    facet_grid(dv ~ term, switch = "y", scales = "fixed") + 
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.4, size = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed") + 
    scale_x_continuous(label = no_lead_zero) +
    theme_minimal() + 
    theme(text = element_text(family = "serif"),
          panel.grid.major.y  = element_blank(),
          panel.grid.minor.x  = element_blank(),
          panel.spacing.y =unit(0,"lines"),
          strip.placement = "outside",
          strip.text.y = element_text(face = "bold"),
          axis.text.y = element_text(face = "italic"),
          axis.title.y = ggtext::element_markdown())
dpm_quarter_nni_fit_plot
save_airbnb_fig(dpm_quarter_nni_fit_plot, "fig-a5-dpm-concentration", height = 3, loc = "figure-appendix")

# Excluding London Zone 1

load("./data/output/dpm_quarter_not_zone_1_fit.RData")
dpm_quarter_not_zone_1_fit_plot <- dpm_coef_plot(dpm_quarter_not_zone_1_fit)
dpm_quarter_not_zone_1_fit_plot
save_airbnb_fig(dpm_quarter_not_zone_1_fit_plot, "fig-a6-dpm-zone-1", loc = "figure-appendix")

# Spatial Lag
load("./data/output/dpm_quarter_splag_fit.RData")

dpm_quarter_splag_fit_plot <- dpm_coef_plot(dpm_quarter_splag_fit, scales = "free_x") +
  scale_x_continuous(
    limits = ~ c(min(.x), ceiling(max(.x))),
    breaks = ~ .x[2],
    expand = c(0, 0)
  ) +
  facetted_pos_scales(x = list(
    term == "Active Rentals" ~ scale_x_continuous(breaks = c(-0, 0.2, 0.4),
                                                  limits = c(-0.1, 0.45),
                                                  labels = no_lead_zero),
    term == "Active Rentals (T - 1)" ~ scale_x_continuous(breaks = c(-0, 0.2, 0.4),
                                                          limits = c(-0.1, 0.45),
                                                          labels = no_lead_zero),
    term == "Splag Active Rentals" ~ scale_x_continuous(breaks = c(-0.005, 0, 0.005),
                                                        limits = c(-0.006, 0.006),
                                                        labels = no_lead_zero),
    term == "Splag Active Rentals (T - 1)" ~ scale_x_continuous(breaks = c(-0.005, 0, 0.005),
                                                                limits = c(-0.006, 0.006),
                                                                labels = no_lead_zero)))
dpm_quarter_splag_fit_plot
save_airbnb_fig(dpm_quarter_splag_fit_plot, "fig-a7-dpm-splag", loc = "figure-appendix")

# Policing

# FIG A8: Police stops
load("./data/output/dpm_quarter_ss_fit.RData")
dpm_quarter_ss_fit_plot <- dpm_coef_plot(dpm_quarter_ss_fit, scales = "free_x")
dpm_quarter_ss_fit_plot
save_airbnb_fig(dpm_quarter_ss_fit_plot, "fig-a8-dpm-stops", loc = "figure-appendix")

# FIG A9: Police patrols
load("./data/output/dpm_half_patrols_fit.RData")
dpm_half_patrols_fit_plot <- dpm_coef_plot(dpm_half_patrols_fit)
dpm_half_patrols_fit_plot
save_airbnb_fig(dpm_half_patrols_fit_plot, "fig-a9-dpm-patrols", loc = "figure-appendix")

# Fig A10: DPM ward-year 
load("./data/output/dpm_year_ce_imputed_2lvl_fit.RData")
dpm_ward_year_plot <- dpm_year_ce_imputed_2lvl_fit %>%
  mutate(term = str_replace_all(term, c("std_ce_imputed_2lvl" = "Collective\nEfficacy",
                                        "std_abnb_active_rentals" = "Active\nRentals"))) %>%
  dpm_coef_plot(scales = "free_x") +
  scale_x_continuous(
    breaks = c(-0.05, 0, 0.05),
    limits = c(-0.1,  0.1),
    labels = no_lead_zero
  ) 
dpm_ward_year_plot
save_airbnb_fig(dpm_ward_year_plot, "fig-a10-dpm-year", loc = "figure-appendix")

# FIG A11: DPM half-year with price
load("./data/output/dpm_year_ce_medianprop_fit.RData")
dpm_year_ce_medianprop_fit_plot <- dpm_year_ce_medianprop_fit%>%
  mutate(term = str_replace_all(term, c("std_ce_imputed_2lvl" = "Collective\nEfficacy",
                                        "std_abnb_active_rentals" = "Active\nRentals",
                                        "log_std_rpp_median_price_aw" = "log(Price)"))) %>%
  dpm_coef_plot(scales = "free_x") +
  scale_x_continuous(
    limits = ~ c(min(.x), ceiling(max(.x))),
    breaks = ~ .x[2],
    expand = c(0, 0)
  ) +
  facetted_pos_scales(x = list(
    term == "Active\nRentals" ~ scale_x_continuous(breaks = c(-0.05, 0, 0.05),
                                                   limits = c(-0.09, 0.09),
                                                   labels = no_lead_zero),
    term == "Active\nRentals (T - 1)" ~ scale_x_continuous(breaks = c(-0.05, 0, 0.05),
                                                           limits = c(-0.09, 0.09),
                                                           labels = no_lead_zero),
    term == "Collective\nEfficacy" ~ scale_x_continuous(breaks = c(-0.05, 0, 0.05),
                                                        limits = c(-0.09, 0.09),
                                                        labels = no_lead_zero),
    term == "Collective\nEfficacy (T - 1)" ~ scale_x_continuous(breaks = c(-0.05, 0, 0.05),
                                                                limits = c(-0.09, 0.09),
                                                                labels = no_lead_zero),
    term == "Log(Price)" ~ scale_x_continuous(breaks = c(-0.3, 0, 0.3),
                                              limits = c(-0.5, 0.5),
                                              labels = no_lead_zero),
    term == "Log(Price) (T - 1)" ~ scale_x_continuous(breaks = c(-0.3, 0, 0.3),
                                                      limits = c(-0.5, 0.5),
                                                      labels = no_lead_zero)))
dpm_year_ce_medianprop_fit_plot
save_airbnb_fig(dpm_year_ce_medianprop_fit_plot, "fig-a11-dpm-medianprop", loc = "figure-appendix")

# FIG A12: DPM CE and Airbnb
load("./data/output/dpm_year_ce_abnb_fit.RData")
dv_ce <- ggplot(dpm_year_ce_abnb_fit %>% 
                  filter(dv == "Collective Efficacy" & !str_detect(term, "Collective Efficacy")), aes(x = estimate, y = spec)) + 
  ylab("*Specification*") + xlab(NULL) +
  geom_point() + 
  facet_grid( ~ term, switch = "y") + 
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), size = 0.2, height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  theme_minimal() + 
  ggtitle("Outcome:\nCollective Efficacy") +
  theme(text = element_text(family = "serif"),
        panel.grid.major.y  = element_blank(),
        panel.grid.minor.x  = element_blank(),
        panel.spacing.y =unit(0,"lines"),
        strip.placement = "outside",
        strip.text.y = element_text(face = "bold"),
        axis.text.y = element_text(face = "italic"),
        axis.title.y = ggtext::element_markdown()) +
  scale_x_continuous(breaks = c(-0.1, 0, 0.1), labels = no_lead_zero)

dv_abnb <- ggplot(dpm_year_ce_abnb_fit %>% 
                    filter(dv == "Active Rentals" & !str_detect(term, "Active Rentals")), aes(x = estimate, y = spec)) + 
  ylab(NULL) + xlab(NULL) +
  geom_point() + 
  facet_grid( ~ term, switch = "y") + 
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), size = 0.2, height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  theme_minimal() +
  ggtitle("Outcome:\nLog of Active Airbnb Rentals") +
  theme(text = element_text(family = "serif"),
        panel.grid.major.y  = element_blank(),
        panel.grid.minor.x  = element_blank(),
        panel.spacing.y =unit(0,"lines"),
        strip.placement = "outside",
        strip.text.y = element_text(face = "bold"),
        axis.text.y = element_blank(),
        axis.title.y = ggtext::element_markdown()) +
  scale_x_continuous(breaks = c(0, 0.01, 0.02), labels = no_lead_zero)
dpm_yearly_ce_abnb_plot <- dv_ce + dv_abnb
dpm_yearly_ce_abnb_plot
save_airbnb_fig(dpm_yearly_ce_abnb_plot, "fig-a12-yearly-ce-abnb", width = 6, height = 3, loc = "figure-appendix")

# Half-Yearly Estimates by Property Type

load("./data/output/dpm_half_diffprops_fit.RData")
dpm_half_diffprops_fit_plot <- dpm_coef_plot(dpm_half_diffprops_fit, con_only = TRUE) 
dpm_half_diffprops_fit_plot
save_airbnb_fig(dpm_half_diffprops_fit_plot, "fig-a13-dpm-diffpropsyear", height = 3, loc = "figure-appendix")


# Perceived Disorder

load("./data/output/dpm_half_dis_fit.RData")
dpm_half_dis_fit_plot <- dpm_half_dis_fit %>%
  mutate(term = str_replace_all(term, c("dis" = "Perceived\nDisorder",
                                        "std_abnb_active_rentals" = "Active\nRentals"))) %>%
  dpm_coef_plot(scales = "free_x") +
  scale_x_continuous(breaks = c(-0.05, 0, 0.05, 0.1),
                     limits = c(-0.09, 0.14),
                     labels = no_lead_zero)
dpm_half_dis_fit_plot
save_airbnb_fig(dpm_half_dis_fit_plot, "fig-a14-dpm-percdis", width = 6.5, height = 6, loc = "figure-appendix")


# Effects on Collective Efficacy by Property Type

load("./data/output/dpm_half_ce_diffprops_fit.RData")
dpm_half_ce_diffprops_fit_plot <- ggplot(dpm_half_ce_diffprops_fit %>% 
         mutate(term = str_replace(term, " Room", "\nRoom"),
                term = str_replace(term, " Apt", "\nApt")) |>
         filter(dv == "Collective Efficacy" & !str_detect(term, "Collective Efficacy")), aes(x = estimate, y = spec)) +
  ylab("*Specification*") + xlab(NULL) +
  geom_point() + 
  facet_grid( ~ term, switch = "y") + 
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), size = 0.2, height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  theme_minimal(base_size = 10) + 
  ggtitle("Outcome:\nCollective Efficacy") +
  theme(text = element_text(family = "serif"),
        panel.grid.major.y  = element_blank(),
        panel.grid.minor.x  = element_blank(),
        panel.spacing.y =unit(0,"lines"),
        strip.placement = "outside",
        strip.text.y = element_text(face = "bold"),
        axis.text.y = element_text(face = "italic"),
        axis.title.y = ggtext::element_markdown()) +
  scale_x_continuous(breaks = c(-0.1, 0, 0.1), label = no_lead_zero)
dpm_half_ce_diffprops_fit_plot
save_airbnb_fig(dpm_half_ce_diffprops_fit_plot, "fig-a15-year-diffprops", width = 6, height = 3, loc = "figure-appendix")

# Prop vals

load("./data/output/dpm_half_ce_medianprop_fit.RData")
dpm_half_ce_medianprop_fit_plot <- dpm_half_ce_medianprop_fit %>%
  mutate(term = str_replace(term, "std_collective_efficacy", "Collective\nEfficacy"),
         term = str_replace(term, "log_std_rpp_median_price_aw", "Property\nPrices"),
         term = str_replace(term, "std_abnb_active_rentals", "Active\nRentals")) %>%
  dpm_coef_plot() +
  scale_x_continuous(breaks = c(-0.1, 0, 0.1),
                     label = no_lead_zero)
dpm_half_ce_medianprop_fit_plot
save_airbnb_fig(dpm_half_ce_medianprop_fit_plot, "fig-a16-ce-propvals", width = 6.5, height = 6, loc = "figure-appendix")
