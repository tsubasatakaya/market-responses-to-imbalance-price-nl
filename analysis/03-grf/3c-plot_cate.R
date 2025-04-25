source("src/R/setup.R")
source("src/R/functions.R")

# model_save_path <- file.path(output_path, "models", "grf")
data_save_path <- file.path(output_path, "data", "grf")

#---------------------------------------------------------------------------------
# Load train data
#---------------------------------------------------------------------------------
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

#---------------------------------------------------------------------------------
# Plot histogram of CATEs
#---------------------------------------------------------------------------------
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
