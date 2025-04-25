source("src/R/setup.R")
source("src/R/functions.R")

#-------------------------------------------------------------------------------
# Lag setup
#-------------------------------------------------------------------------------
dv_lags <- 1:4
endo_lags <- 1:4
iv_lags <- 1:4
reg_dummy_lags <- 1:4

dv_autoregressive_cov <- paste0(dv, "_lag_", dv_lags, "_sp")
endo_autoregressive_cov <- paste0(endo, "_lag_", endo_lags, "_sp")
iv_autoregressive_cov <- list(
  "pos" = paste0(iv_set[["pos"]], "_lag_", iv_lags, "_sp"),
  "neg" = paste0(iv_set[["neg"]], "_lag_", iv_lags, "_sp"),
  "comb" = c(paste0(iv_set[["comb"]][1], "_lag_", iv_lags, "_sp"),
             paste0(iv_set[["comb"]][2], "_lag_", iv_lags, "_sp"))
)
reg_dummy_autoregressive_cov <- paste0("regulation_state_pos_dummy", "_lag_", 
                                       endo_lags, "_sp")

#-------------------------------------------------------------------------------
# Data preparation
#-------------------------------------------------------------------------------
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
  add_lags(dv, dv_lags) |> 
  add_lags(endo, endo_lags) |> 
  add_lags(iv_set[["pos"]], iv_lags) |> 
  add_lags(iv_set[["neg"]], iv_lags) |> 
  add_lags(iv_set[["comb"]], iv_lags) |>
  add_lags("regulation_state_pos_dummy", reg_dummy_lags) |> 
  # Drop missing values
  drop_na(any_of(c(dv, endo, unname(unlist(iv_set)), cov)))

imb_data <- imb_data |> 
  filter(!(regulation_state %in% c(0, 2)))
  
pos_data <- imb_data |> 
  filter(regulation_state_pos_dummy == 1)
neg_data <- imb_data |> 
  filter(regulation_state_pos_dummy == 0)

#-------------------------------------------------------------------------------
# Modelsummary parameters
#-------------------------------------------------------------------------------
cm <- c("fit_imbalance_price" = "Imbalance settlement price")
gof_f <- function(x) format(round(x, 2), big.mark = ",")
gm <- list(
  list("raw" = "nobs", "clean" = "Observations", "fmt" = gof_f)
)


#-------------------------------------------------------------------------------
# Fit models
#-------------------------------------------------------------------------------
#=================================
# Conditional IV 1 (with IV lags)
#=================================
iv_model_tags <- c("pos", "neg", "comb")
conditional_iv_1_res <- list()
data_list <- list(pos_data, neg_data, imb_data)

for (i in seq_along(iv_model_tags)) {
  model_tag <- iv_model_tags[i]
  if (model_tag == "comb") {
    cov_all <- c(cov, 
                 iv_autoregressive_cov[[model_tag]],
                 "regulation_state_pos_dummy",
                 reg_dummy_autoregressive_cov)
  } else {
    cov_all <- c(cov,
                 iv_autoregressive_cov[[model_tag]]
                 )
  }
  res <- fit_iv_linear(data_list[[i]] |> drop_na(any_of(cov_all)),
                       dv = dv,
                       endo = endo,
                       cov = cov_all,
                       iv = iv_set[[model_tag]],
                       vcov = vcov_NW(time = ~ delivery_start))
  conditional_iv_1_res[[paste0("(", i, ")")]] <- res
}

# Extract F-statistics from model objects
f_stats_civ_1 <- map_dbl(conditional_iv_1_res, 
                         ~ round(fitstat(.x, "ivf")[["ivf1::imbalance_price"]][["stat"]], 
                                 2))

#=================================
# Conditional IV 2 (with IV lags and outcome and treatment lags)
#=================================
conditional_iv_2_res <- list()

for (i in seq_along(iv_model_tags)) {
  model_tag <- iv_model_tags[i]
  if (model_tag == "comb") {
    cov_all <- c(cov, 
                 iv_autoregressive_cov[[model_tag]],
                 dv_autoregressive_cov,
                 endo_autoregressive_cov,
                 "regulation_state_pos_dummy",
                 reg_dummy_autoregressive_cov)
  } else {
    cov_all <- c(cov,
                 iv_autoregressive_cov[[model_tag]],
                 dv_autoregressive_cov,
                 endo_autoregressive_cov)
  }
  res <- fit_iv_linear(data_list[[i]] |> drop_na(any_of(cov_all)),
                       dv = dv,
                       endo = endo,
                       cov = cov_all,
                       iv = iv_set[[model_tag]],
                       vcov = vcov_NW(time = ~ delivery_start))
  conditional_iv_2_res[[paste0("(", i+3, ")")]] <- res
}

# Extract F-statistics from model objects
f_stats_civ_2 <- map_dbl(conditional_iv_2_res, 
                         ~ round(fitstat(.x, "ivf")[["ivf1::imbalance_price"]][["stat"]], 
                                 2))


#-------------------------------------------------------------------------------
# Summary table
#-------------------------------------------------------------------------------
add_rows <- tibble(term = "First-stage F-statistic",
                   col1 = f_stats_civ_1[1],
                   col2 = f_stats_civ_1[2],
                   col3 = f_stats_civ_1[3],
                   col4 = f_stats_civ_2[1],
                   col5 = f_stats_civ_2[2],
                   col6 = f_stats_civ_2[3],
)
attr(add_rows, "position") <- 3

modelsummary(c(conditional_iv_1_res, conditional_iv_2_res),
             fmt = 2,
             coef_map = cm, gof_map = gm,
             add_rows = add_rows,
             output = "gt") |> 
  tab_spanner(label = "Short system", 
              columns = seq(2, 5, 3),
              gather = FALSE) |>
  tab_spanner(label = "Long system", 
              columns = seq(3, 6, 3),
              gather = FALSE) |> 
  tab_spanner(label = "Combined", 
              columns = seq(4, 7, 3),
              gather = FALSE) |> 
  rows_add(
    starts_with(" ") ~ "Instrument",
    c(`(1)`, `(4)`) ~ "Z_up",
    c(`(2)`, `(5)`) ~ "Z_down",
    c(`(3)`, `(6)`)  ~ "Z_up, Z_down",
    .before = 3
  ) |> 
  rows_add(
    starts_with(" ") ~ "Lagged instrument",
    `(1)` ~ "X",
    `(2)` ~ "X",
    `(3)` ~ "X",
    `(4)` ~ "X",
    `(5)` ~ "X",
    `(6)` ~ "X",
    .before = 4
  ) |>
  rows_add(
    starts_with(" ") ~ "Lagged treatment & outcome",
    `(1)` ~ "",
    `(2)` ~ "",
    `(3)` ~ "",
    `(4)` ~ "X",
    `(5)` ~ "X",
    `(6)` ~ "X",
    .before = 5
  ) |>
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_spanners()
  ) |> 
  tab_options(table.font.size = "9pt",
              table.width  = pct(100),
              latex.use_longtable = TRUE) |> 
  cols_width(!starts_with(" ") ~ px(75)) |>
  gtsave("robustness_with_lags.tex", path = file.path(output_path, "tables"))

