source("src/R/setup.R")
source("src/R/functions.R")

# model_save_path <- file.path(output_path, "models", "grf")
data_save_path <- file.path(output_path, "data", "grf")

#-------------------------------------------------------------------------------
# Load train data
#-------------------------------------------------------------------------------
directions <- c("pos", "neg")

train_data_all <- list()
for (direc in directions) {
  train_data_all[[direc]] <- list()
  
  for (df in c("ts", "X", "y", "T", "Z")) {
    file_path <- file.path(data_save_path, 
                           paste0(direc, "_", df, ".csv"))
    
    train_data_all[[direc]][[df]] <- vroom(file_path, delim = ",")
  }
}

#-------------------------------------------------------------------------------
# Plot histogram of CATEs
#-------------------------------------------------------------------------------
cate_train_pos <- vroom(file.path(data_save_path, "pos_cate_train_data.csv")) |> 
  filter(tau_hat >= quantile(tau_hat, 0.005) & tau_hat <= quantile(tau_hat, 0.995)) |> 
  mutate(direction = "pos")
cate_train_neg <- vroom(file.path(data_save_path, "neg_cate_train_data.csv")) |> 
  filter(tau_hat >= quantile(tau_hat, 0.005) & tau_hat <= quantile(tau_hat, 0.995)) |> 
  mutate(direction = "neg")

cate_median <- tibble(
  tau_hat_median = c(median(cate_train_pos$tau_hat), median(cate_train_neg$tau_hat)),
  direction = c("pos", "neg")
) |> 
  mutate(tau_hat_median = round(tau_hat_median, 2),
         direction = factor(direction, levels = directions),
         text_position = case_when(direction == "pos" ~ tau_hat_median - 0.04,
                                   direction == "neg" ~ tau_hat_median - 0.2))


cate_train_hist_plot <- bind_rows(cate_train_pos, cate_train_neg) |> 
  mutate(direction = factor(direction, levels = directions)) |> 
  ggplot(aes(x = tau_hat, fill = direction)) +
  geom_histogram(bins = 20, color = "#e9ecef", alpha = 0.7) +
  geom_segment(data = cate_median, 
               aes(x = tau_hat_median, xend = tau_hat_median,
                   y = Inf, yend = -Inf),
               linetype = "dashed") +
  geom_text(data = cate_median, 
            aes(x = text_position, 
                y = 7500, 
                label = paste("Median = ", tau_hat_median)),
            hjust = 1, size = 3.5, color = "black") +
  facet_wrap(~ direction,
             labeller = labeller(
               direction = c("pos" = "Short system",
                             "neg" = "Long system")),
             scales = "free_x"
  ) +
  scale_fill_manual(values = c("pos" = "#E76F51", "neg" = "#2A9D8F")) +
  scale_x_continuous(
    labels = scales::number_format(accuracy = 0.01,
                                   decimal.mark = '.')) +
  labs(x = "Conditional elasticity (MW per €/MWh)", y = "Frequency") +
  theme_bw() +
  theme_custom() +
  theme(legend.position = "none",
        plot.margin = margin(5, 10, 10, 10))

ggsave(file.path(output_path, "figures/cate_train_histogram.pdf"),
       cate_train_hist_plot, width = 6.6, height = 4, 
       units = "in", dpi = 150, useDingbats = TRUE)


#-------------------------------------------------------------------------------
# Plot test CATEs as a function of t-1 imbalance and t-1 imbalance price
#-------------------------------------------------------------------------------
data_file_tag <- "imb_1_isp_1"
cate_test_data <- list(
  "pos" = vroom(file.path(data_save_path, 
                          paste0("pos_cate_test_data_", data_file_tag, "_quantile", ".csv"))),
  "neg" = vroom(file.path(data_save_path, 
                          paste0("neg_cate_test_data_", data_file_tag, "_quantile", ".csv")))
)

continuous_col <- "imbalance_lag_1_sp"
discrete_col <- "imbalance_price_lag_1_sp"
quantiles <- c(0.25, 0.75)

pos_plot_data <- create_marginal_df(X_orig = train_data_all[["pos"]][["X"]],
                                    test_df = cate_test_data[["pos"]],
                                    discrete_col = discrete_col,
                                    quantiles = quantiles)
neg_plot_data <- create_marginal_df(X_orig = train_data_all[["neg"]][["X"]],
                                    test_df = cate_test_data[["neg"]],
                                    discrete_col = discrete_col,
                                    quantiles = quantiles)

imb_isp_pos_cate_plot <- plot_cate_facet(
  plot_df = pos_plot_data, 
  continuous_col = continuous_col, 
  discrete_col = discrete_col, 
  discrete_quantiles = quantiles,
  color = "#E76F51", 
  discrete_col_label = "t-1 imbalance price", 
  discrete_col_unit = "€/MWh"
) +
  labs(x = "t-1 system imbalance (MW)",
       y = "Conditional elasticity (MW per €/MWh)")

imb_isp_pos_scatter_plot <- plot_2d_density_scatter(
  plot_df = train_data_all[["pos"]][["X"]],
  x = continuous_col,
  y = discrete_col,
) +
  scale_y_continuous(limits = c(-500, 1200),
                     breaks = seq(-500, 1200, 500)) +
  labs(x = "t-1 system imbalance (MW)",
       y = "t-1 imbalance price (€/MWh)",
       title = "Short system") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text = element_text(size = 12),
        axis.title = element_text(margin = margin(0,0,0,0),
                                  size = 14),
        plot.margin = unit(c(0, 0.5, 0, 0.2), "cm"))

imb_isp_neg_cate_plot <- plot_cate_facet(
  plot_df = neg_plot_data, 
  continuous_col = continuous_col, 
  discrete_col = discrete_col, 
  discrete_quantiles = quantiles,
  color = "#2A9D8F", 
  discrete_col_label = "t-1 imbalance price", 
  discrete_col_unit = "€/MWh"
) +
  labs(x = "t-1 system imbalance (MW)",
       y = "Conditional elasticity (MW per €/MWh)")

imb_isp_neg_scatter_plot <- plot_2d_density_scatter(
  plot_df = train_data_all[["neg"]][["X"]],
  x = continuous_col,
  y = discrete_col,
) +
  scale_y_continuous(limits = c(-1200, 600),
                     breaks = seq(-1000, 1000, 500)) +
  labs(x = "t-1 system imbalance (MW)",
       y = "t-1 imbalance price (€/MWh)",
       title = "Long system") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text = element_text(size = 12),
        axis.title = element_text(margin = margin(0,0,0,0),
                                  size = 14),
        plot.margin = unit(c(0, 0.2, 0, 0.5), "cm"))

ggsave(file.path(output_path, "figures/imb1_isp1_pos_cate_plot.pdf"),
       imb_isp_pos_cate_plot, width = 8, height = 4.6, 
       units = "in", dpi = 150, useDingbats = TRUE)
ggsave(file.path(output_path, "figures/imb1_isp1_neg_cate_plot.pdf"),
       imb_isp_neg_cate_plot, width = 8, height = 4.6, 
       units = "in", dpi = 150, useDingbats = TRUE)

imb_isp_scatter_combined <- grid.arrange(imb_isp_pos_scatter_plot, imb_isp_neg_scatter_plot,
                                         ncol = 2)
ggsave(file.path(output_path, "figures/imb1_isp1_point_density_plot.pdf"),
       imb_isp_scatter_combined, width = 11.7, height = 8.3, 
       units = "in", dpi = 150, useDingbats = TRUE)
