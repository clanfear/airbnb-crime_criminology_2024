standardize <- function(x){
  out <- (x - mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE)
  return(out)
}

`%!in%` <- Negate(`%in%`)

list_missing <- function(x){
  data.frame(var     = names(x),
             valid   = sapply(x, \(x) sum(!is.na(x))),
             missing = sapply(x, \(x) sum(is.na(x))))
}

log_na <- function(x){
  return(ifelse(x <= 0, NA, log(x)))
}

st_erase <- function(x, y) {
  st_difference(x, st_make_valid(st_union(st_combine(y))))
}

get_neighbors <- function(df, id, snap = 0.001){
  out <- df %>%
    select(all_of(id), "geometry") |>
    # Reclassing poly2nb lets it be used in a normal mutate call
    mutate(neighbors = magrittr::set_class(poly2nb(geometry, snap = snap), "list")) |>
    st_drop_geometry() |>
    unnest(neighbors) |>
    mutate(neighbors = df[[id]][neighbors])
  return(out)
}

run_dpm <- function(formula_frame, analysis_data, missing = "ML", estimator = "MLR",  ...){
  out_frame <- mutate(tibble(formula_frame), 
                      models = furrr::future_map(form, function(.x){ 
                        dpm_out <- dpm(formula(.x), data = analysis_data, estimator = estimator, missing = missing, ...)
                        return(dpm_out)
                      }, .options = furrr::furrr_options(seed = T), .progress = TRUE
                      )) %>%
    mutate(fit    = map(models, ~ lavaan::fitmeasures(.x)),
           coefs  =  map(models, ~broom::tidy(.x, conf.int = TRUE))) %>%
    select(-models,  -form) %>%
    mutate(RMSEA  = map_dbl(fit, ~.x["rmsea"]),
           SRMR   = map_dbl(fit, ~.x["srmr"]),
           BIC    = map_dbl(fit, ~.x["bic"]),
           N      = map_dbl(fit, ~.x["ntotal"])) %>%
    select(-fit) %>%
    unnest(coefs)
  return(out_frame)
}

lme_reliability_2lvl <- function(x){
  var_components <- insight::get_variance(x)
  t00 <- var_components$var.intercept[[1]]
  s2  <- var_components$var.residual
  n_count <- table(insight::get_random(x))
  J <- length(n_count) # number of neighbs
  sum(t00 / (t00 + s2 / n_count)) / J
}

lme_reliability_3lvl <- function(x){
  # This is RRK 1991 calculation
  var_components <- insight::get_variance(x)
  tb <- var_components$var.intercept[[2]]
  tp <- var_components$var.intercept[[1]]
  s2  <- var_components$var.residual
  rel <- x@frame %>% 
    count(year_ward, id) %>%
    group_by(year_ward) %>%
    summarize(jk = n(),
              njk = sum(n)) %>%
    mutate(reliability = tb / (tb + tp/jk + s2/(njk))) %>% 
    pull(reliability)
  return(mean(rel))
}

no_lead_zero <- function(x){
  x <- as.character(x)
  x <-str_replace(x, "^0\\.", ".")
  x <-str_replace(x, "^-0\\.", "-.")
  return(x)
}

dpm_coef_plot <- function(x, hide_lag = TRUE, con_only = FALSE, scales = "fixed"){
  dv_levels <- c("Robbery", "Burglary", "Theft", "ASB", "Violence", "Harm")
  plot_data <- x %>%
    select(dv, spec, term, estimate, std.error, conf.low, conf.high) %>%
    mutate(term = ifelse(str_detect(term, "dp|dlg") & str_detect(term, "t - 1"), "crime (t - 1)", term),
           across(c(dv, term), ~ str_to_title(str_replace_all(str_remove_all(., "(log_)|(std_)|(dlg_|dp_|abnb_)|(_aw$)?"), "_", " "))),
           spec = fct_rev(factor(str_to_title(spec), levels = c("Con", "Lag", "Both"))),
           dv = fct_relevel(str_replace_all(dv, c("Violence Harm" = "Harm", "Asb" = "ASB")), dv_levels)) |>
    mutate(term = ifelse(term=="Entire Home Apt", "Entire Property", term))
  if(hide_lag){
    plot_data <- plot_data %>% filter(term != "Crime (T - 1)")
  }
  if(con_only){
    plot_data <- plot_data %>% 
      filter(spec == "Both" & !str_detect(term, "- 1")) %>% 
      mutate(dv = fct_rev(dv)) %>%
      mutate(graybar = ifelse(dv %in% c("Burglary", "ASB", "Harm"), "black", "white"))
    ggplot(plot_data, aes(x = estimate, y = dv)) + 
      geom_tile(aes(alpha = graybar), 
                width = Inf, height = 1, fill = "black") +
      scale_alpha_manual(values = c("black" = 0.3, "white" = 0)) +
      ylab("**Crime type**") + xlab(NULL) +
      geom_point() + 
      facet_grid( ~ term, scales = scales) + 
      geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, size = 0.2) +
      geom_vline(xintercept = 0, linetype = "dashed") + 
      scale_x_continuous(label = no_lead_zero) +
      theme_minimal() + 
      theme(text = element_text(family = "serif"),
            panel.grid.major.y  = element_blank(),
            panel.grid.minor.x  = element_blank(),
            panel.spacing.y = unit(0,"lines"),
            strip.placement = "outside",
            strip.text.y = element_text(face = "bold"),
            # axis.text.y = element_text(face = "bold"),
            axis.title.y = ggtext::element_markdown(),
            legend.position = "none")
  } else {
  ggplot(plot_data, aes(x = estimate, y = spec, group = dv)) + 
    geom_rect(data = filter(plot_data, dv %in% dv_levels[c(2,4,6)] & spec == "Both"), fill = "black", xmin = -Inf, xmax = Inf,
              ymin = -Inf, ymax = Inf, alpha = 0.1) +
    ylab("**Crime type** and *Specification*") + xlab(NULL) +
    geom_point() + 
    facet_grid(dv ~ term, switch = "y", scales = scales) + 
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
  }
}
