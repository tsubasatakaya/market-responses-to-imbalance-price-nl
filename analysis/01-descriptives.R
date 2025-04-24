source("src/R/setup.R")
source("src/R/functions.R")


#-------------------------------------------------------------------------------
# Data preparation
#-------------------------------------------------------------------------------
imb_data <- get_processed_base_data()

#-------------------------------------------------------------------------------
# Plot system imbalance vs. imbalance price
#-------------------------------------------------------------------------------
point_density_plot <- ggplot(imb_data, aes(x = imbalance, y = imbalance_price)) +
  geom_pointdensity(size = 0.5) +
  scale_color_viridis(name = "Number of neighbors") +
  ylim(-1000, 2000) +
  labs(x = "System imbalance (MW)", y = "Imbalance settlement price (€/MWh)") +
  facet_wrap(
    ~ regulation_state_pos_dummy,
    scales = "free_x",
    labeller = labeller(
      regulation_state_pos_dummy = c("0" = "Long system", "1" = "Short system")
    )
  ) +
  theme_bw() +
  theme_custom() +
  theme(
    panel.border = element_rect(color = "grey", fill = NA),
    legend.position = "bottom"
  )

ggsave(file.path(output_path, "figures/imb_isp_point_density_plot.pdf"),
       point_density_plot, width = 7, height = 5, 
       units = "in", dpi = 300, useDingbats = TRUE)


#-------------------------------------------------------------------------------
# Descriptive statistics
#-------------------------------------------------------------------------------
iv_set <- iv_set[["comb"]]
selected_covs <- c("da_price",
                   "non_usable_capacity",
                   "solar_forecast_error",
                   "wind_forecast_error",
                   "load_forecast_error")
var_list <- c(dv, endo, iv_set, selected_covs)


pos_data <- imb_data |> filter(regulation_state_pos_dummy == 1)
neg_data <- imb_data |> filter(regulation_state_pos_dummy == 0)

nobs_pos <- nrow(pos_data)
nobs_neg <- nrow(neg_data)

summary_data <- create_summary_data(pos_data, dv, endo, iv_set, selected_covs) |> 
  full_join(create_summary_data(neg_data, dv, endo, iv_set, selected_covs),
            by = c("variable", "category"), suffix = c("_short", "_long"))

summary_data |> 
  gt(groupname_col = "category", rowname_col = "variable") |> 
  cols_label(
    variable = "",
    mean_short = "Mean",
    mean_long = "Mean",
    sd_short = "Standard deviation",
    sd_long = "Standard deviation",
    min_short = "Min",
    min_long = "Min",
    max_short = "Max",
    max_long = "Max"
  ) |> 
  tab_spanner(label = "Short system",
              columns = 2:5) |> 
  tab_spanner(label = "Long system",
              columns = 6:10) |> 
  row_group_order(groups = c("Outcome", "Treatment", 
                             "Instruments", "Covariates")) |> 
  cols_align(align = "left",
             columns = "variable") |> 
  cols_align(align = "center",
             columns = 2:10) |>
  tab_stub_indent(
    rows = everything(),
    indent = 3
  ) |> 
  rows_add(variable = "Observations",
           mean_short = nobs_pos,
           mean_long = nobs_neg,
           c(sd_short, min_short, max_short,
             sd_long, min_long, max_long) ~ NA) |> 
  fmt_number(columns = c("mean_short", "mean_long"),
             rows = 10,
             decimals = 0) |>
  fmt_number(columns = c("min_short", "max_short", "min_long", "max_long"),
             rows = 6:9,
             decimals = 0) |> 
  sub_values(rows = 3, columns = 6:10, values = 0, replacement = "-") |> 
  sub_values(rows = 4, columns = 2:5, values = 0, replacement = "-") |> 
  sub_missing(missing_text = "") |> 
  tab_options(table.border.top.style = "hidden",
              heading.align = "left",
              table.font.size = "9pt",
              table.width = pct(60),
              latex.use_longtable = TRUE) |> 
  cols_width(!variable ~ px(50)) |> 
  gtsave("sample_stats.tex", path = file.path(output_path, "tables"))


