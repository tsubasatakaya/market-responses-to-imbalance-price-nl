#-----------------------------
# Common plot setting
#-----------------------------
theme_custom <- function() {
  theme(
    axis.title = element_text(size = 12),
    axis.title.x = element_text(margin = margin(7,0,0,0)),
    axis.title.y = element_text(margin = margin(0,7,0,0)),
    axis.text = element_text(size = 10),
    strip.text.x = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
  )
}

#-----------------------------
# Descriptives
#-----------------------------

create_summary_data <- function(data, dv, endo, iv_set, covs) {
  summary_data <- data |> 
    select(all_of(c(dv, endo, iv_set, covs))) |> 
    pivot_longer(everything(), names_to = "variable", values_to = "value") |> 
    mutate(variable = factor(variable, levels = c(dv, endo, iv_set, covs))) |> 
    group_by(variable) |> 
    summarize(mean = mean(value),
              sd = sd(value),
              min = min(value),
              max = max(value)) |> 
    mutate(across(where(is.numeric), ~ round(.x, 2))) |> 
    mutate(category = case_when(variable == dv ~ "Outcome",
                                variable == endo ~ "Treatment",
                                variable %in% iv_set ~ "Instruments",
                                variable %in% covs ~ "Covariates")) |> 
    mutate(variable = case_match(
      variable,
      dv ~ "System imbalance (MW)",
      endo ~ "Imbalance price (€/MWh)",
      iv_set[1] ~ "Upward aFRR price (€/MWh)",
      iv_set[2] ~ "Downward aFRR price (€/MWh)",
      "da_price" ~ "Day-ahead prices (€/MWh)",
      "non_usable_capacity" ~ "Unplanned outage capacity (MW)",
      "solar_forecast_error" ~ "Solar forecast errors (MW)",
      "wind_forecast_error" ~ "Wind forecast errors (MW)",
      "load_forecast_error" ~ "Load forecast errors (MW)")
    )
  return(summary_data)
}

#-----------------------------
# Data wrangling
#-----------------------------

impute_null_outlier <- function(df, x, sd) {
  return (
    df |> 
      mutate({{x}} := case_when(
        ({{x}} < mean({{x}}, na.rm = TRUE) - sd({{x}}, na.rm = TRUE) * sd) |
          ({{x}} > mean({{x}}, na.rm = TRUE) + sd({{x}}, na.rm = TRUE) * sd) ~ NA,
        TRUE ~ {{x}}
      )
      )
  )
}

add_lags <- function(df, vars, lags) {
  lag_func <- setNames(map(lags, ~ \(x) lag(x, .x)), as.character(lags))
  
  return(
    df |> 
      mutate(across(all_of(vars), lag_func, .names = "{.col}_lag_{.fn}_sp"))
  )
}


#-----------------------------
# 2SLS
#-----------------------------

fit_iv_linear <- function(data, dv, endo, cov, iv, vcov) {
  formula <- as.formula(
    paste0(dv, "~",
           paste0(cov, collapse = "+"),
           "|",
           paste0(endo,
                  "~",
                  paste0(iv, collapse = "+")))
  )
  res <- feols(formula, data = data, vcov = vcov)
  return(res)
}

bulk_fit_iv_linear <- function(data, config_list) {
  res <- purrr::imap(config_list, \(x, idx) {
    data <- data |> 
      drop_na(any_of(unname(unlist(keep(x, is.character)))))
    
    if (nrow(data) == 0) {
      message(glue("Data is empty after discarding NA for {idx}. Will ignore."))
      return(NULL)
    } else {
      return(fit_iv_linear(data, x$dv, x$endo, x$cov, x$iv, vcov = x$std))
    }
  }
  )
  
  names(res) <- names(config_list)
  
  return(res)
}