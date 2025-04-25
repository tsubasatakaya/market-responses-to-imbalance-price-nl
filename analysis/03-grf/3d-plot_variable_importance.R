source("src/R/setup.R")
source("src/R/functions.R")

data_save_path <- file.path(output_path, "data", "grf")

#-------------------------------------------------------------------------------
# Load feature importance data
#-------------------------------------------------------------------------------
directions <- c("pos", "neg")

varimp_all <- list()
for (direc in directions) {
  file_path <- file.path(data_save_path,
                         paste0(direc, "_var_imp.csv"))
  varimp_all[[direc]] <- vroom(file_path) |> 
    mutate(direction = direc)
}
varimp_data <- bind_rows(varimp_all)

#-------------------------------------------------------------------------------
# Plot feature importance
#-------------------------------------------------------------------------------
name_mapping <- c(
  "imbalance_price_lag_1_sp" = "t-1 imbalance price",
  "imbalance_price_lag_2_sp" = "t-2 imbalance price",
  "imbalance_price_lag_3_sp" = "t-3 imbalance price",
  "imbalance_price_lag_4_sp" = "t-4 imbalance price",
  "imbalance_price_lag_96_sp" = "t-96 imbalance price",
  "imbalance_lag_1_sp" = "t-1 System imbalance",
  "imbalance_lag_96_sp" = "t-96 System imbalance",
  "wind_forecast_error" = "Wind forecast error",
  "load_forecast_error" = "Load forecast error",
  "min_down_price" = "Minimum downward aFRR price",
  "max_up_price" = "Maximum upward aFRR price",
  "da_price" = "Day-ahead price"
)
varimp_top10 <- varimp_data |> 
  group_by(direction) |> 
  slice_max(var_imp, n = 10) |> 
  ungroup() |> 
  mutate(name = recode(variable, !!!name_mapping),
         direction = factor(direction, levels = c("pos", "neg")))

varimp_plot <- varimp_top10 |> 
  ggplot(aes(x = reorder_within(name, desc(var_imp), direction), 
             y = var_imp,
             fill = direction)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ direction,
             labeller = labeller(
               direction = c("pos" = "Short system",
                             "neg" = "Long system")),
             scales = "free_x"
  ) +
  scale_x_reordered() +
  scale_fill_manual(values = c("pos" = "#E76F51", "neg" = "#2A9D8F")) +
  labs(x = "",
       y = "Variable importance") +
  theme_bw() +
  theme_custom() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 16),
        plot.margin = margin(10, 10, 10, 10))

ggsave(file.path(output_path, "figures/grf_variable_importance_plot.pdf"),
       varimp_plot, width = 11.7, height = 8.3, 
       units = "in", dpi = 150, useDingbats = TRUE)
