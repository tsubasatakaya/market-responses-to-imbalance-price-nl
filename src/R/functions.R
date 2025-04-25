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

get_processed_base_data <- function() {
  base_data <- vroom(file.path(data_path, "base_15min_data.csv")) |> 
    mutate(delivery_start = with_tz(delivery_start, tzone = "CET")) |> 
    mutate(across(all_of(c("year", "month", "dow", "hour", "qh")), as.factor)) |>
    mutate(delivery_start = with_tz(delivery_start, tzone = "UTC"))
  
  lag_vars <- c(
    "regulation_state_2_dummy",
    "imbalance"
  )
  lags <- c(3, 96)
  
  imb_data <- base_data |> 
    # Drop outliers of DV
    impute_null_outlier(imbalance, sd = 3) |>
    # Create dummies for regulation state 2 and 1
    mutate(
      regulation_state_2_dummy = case_when(regulation_state == 2 ~ 1,
                                           TRUE ~ 0),
      regulation_state_pos_dummy = case_when(regulation_state == 1 ~ 1,
                                             TRUE ~ 0),
    ) |> 
    # Drop settlement periods with regulation state 1/-1 with positive/negative imbalance
    filter((regulation_state_pos_dummy == 1 & imbalance >= 0) |
             (regulation_state_pos_dummy == 0 & imbalance < 0)) |>
    # Create interaction of energy price and regulation state
    mutate(
      up_price_x_pos_dummy = up_weighted_price * regulation_state_pos_dummy,
      down_price_x_neg_dummy = down_weighted_price * (1 - regulation_state_pos_dummy)
    ) |> 
    # Add lag terms
    add_lags(lag_vars, lags) |> 
    # Drop missing values
    drop_na(any_of(c(dv, endo, unname(unlist(iv_set)), cov)))
  
  
  imb_data <- imb_data |> 
    filter(!(regulation_state %in% c(0, 2)))
  
  return(imb_data)
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


#-----------------------------
# GRF
#-----------------------------
get_nearest_value <- function(x, values) {
  values <- unique(values)
  val <- values[which.min(abs(x - values))]
  return(val)
}

get_discrete_test_values <- function(X_orig, test_df, discrete_col, quantiles) {
  # Extract values of discrete_col corresponding to each quantile from X_orig (train data)
  fix_col_quantiles <- quantile(X_orig |> pull(any_of(c(discrete_col))),
                                quantiles)
  # Get values from test_df that are closest to each quantile value
  fix_col_test_vals <- map_dbl(unname(fix_col_quantiles),
                               ~ get_nearest_value(.x, 
                                                   test_df |> 
                                                     pull(any_of(c(discrete_col)))))
  return(fix_col_test_vals)
}

create_marginal_df <- function(X_orig, 
                               test_df, 
                               discrete_col, 
                               quantiles) {
  
  discrete_vals <- get_discrete_test_values(X_orig, test_df, discrete_col, quantiles)
  print(setNames(discrete_vals, quantiles))
  
  res <- tibble()
  for (i in seq_along(quantiles)) {
    marginal_df <- test_df |> 
      filter(!!sym(discrete_col) == discrete_vals[i]) |> 
      mutate(quantile = quantiles[i])
    
    res <- bind_rows(res, marginal_df)
  }
  return(res)
}