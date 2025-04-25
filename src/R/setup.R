#-----------------------------
# Path
#-----------------------------
data_path <- "analysis/data"
output_path <- "output"


#-----------------------------
# Packages
#-----------------------------
packages <-  c(
  "broom", 
  "fastDummies",
  "fixest", 
  "forcats",
  "forecast",
  "ggplot2",
  "ggpointdensity",
  "glue",
  "grf",
  "gridExtra",
  "gt",
  "modelsummary",
  "scales",
  "see",
  "tidytext",
  "tidyverse",
  "viridis",
  "vroom",
  "zoo")

package.check <- lapply(
  packages,
  FUN <-  function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)


#-----------------------------
# Common variable config
#-----------------------------
dv <- "imbalance"
endo <- "imbalance_price"
iv_set <- list(
  "pos" = c("up_weighted_price"),
  "neg" = c("down_weighted_price"),
  "comb" = c("up_price_x_pos_dummy", "down_price_x_neg_dummy")
)

cov <- c(
  "non_usable_capacity",
  "da_price",
  "solar_forecast_error",
  "wind_forecast_error",
  "load_forecast_error",
  "imbalance_price_lag_3_sp",
  "imbalance_lag_3_sp",
  "imbalance_price_lag_96_sp",
  "imbalance_lag_96_sp",
  "regulation_state_2_dummy_lag_3_sp",
  "month",
  "holiday",
  "dow",
  "hour",
  "qh"
)