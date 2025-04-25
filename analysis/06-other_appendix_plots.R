source("src/R/setup.R")
source("src/R/functions.R")

base_data <- vroom(file.path(data_path, "base_15min_data.csv")) |> 
  mutate(delivery_start = with_tz(delivery_start, tzone = "CET")) |> 
  mutate(across(all_of(c("year", "month", "holiday", "dow", "hour", "qh")), as.factor))

#-------------------------------------------------------------------------------
# Partial autocorrelation plot
#-------------------------------------------------------------------------------
common_theme <- function() {
  theme_bw() +
    theme_custom() +
    theme(plot.title = element_text(hjust = 0.5))
}

up_weighted_price_pacf <- base_data |> 
  pull(up_weighted_price) |> 
  ggPacf(lag.max = 60) +
  labs(x = "Lag (quarter-hour)", 
       title = "Upward aFRR price proxy") +
  common_theme()

down_weighted_price_pacf <- base_data |> 
  pull(down_weighted_price) |> 
  ggPacf(lag.max = 60) +
  labs(x = "Lag (quarter-hour)", 
       title = "Downward aFRR price proxy") +
  common_theme()

imbalance_price_pacf <- base_data |> 
  pull(imbalance_price) |> 
  ggPacf(lag.max = 60) +
  labs(x = "Lag (quarter-hour)", 
       title = "Imbalance settlement price") +
  common_theme()

imbalance_pacf <- base_data |> 
  pull(imbalance) |> 
  ggPacf(lag.max = 60) +
  labs(x = "Lag (quarter-hour)", 
       title = "System imbalance") +
  common_theme()

combined_plot <- grid.arrange(up_weighted_price_pacf, down_weighted_price_pacf, 
                              imbalance_price_pacf, imbalance_pacf, ncol = 2, nrow = 2)

ggsave(file.path(output_path, "figures/pacf_plots.pdf"),
       combined_plot, width = 8.6, height = 11, units = "in", 
       dpi = 100, useDingbats = TRUE)

#-------------------------------------------------------------------------------
# Calculate 1-month moving average of ID1 traded volumes for QH products
#-------------------------------------------------------------------------------
id1_vol_ts_pos <- base_data |> 
  filter(regulation_state == 1) |> 
  mutate(
    value = rollapply(id1_vol_quarter_hourly,
                      width = 96 * 30,
                      FUN = mean, 
                      align = "center",
                      na.rm = TRUE,
                      fill = NA),
    
  ) |> 
  select(delivery_start, value)

id1_vol_ts_neg <- base_data |> 
  filter(regulation_state == -1) |> 
  mutate(
    value = rollapply(id1_vol_quarter_hourly,
                      width = 96 * 30,
                      FUN = mean, 
                      align = "center",
                      na.rm = TRUE,
                      fill = NA),
    
  ) |> 
  select(delivery_start, value)

#-------------------------------------------------------------------------------
# Plot moving average
#-------------------------------------------------------------------------------
plot_data <- 
  bind_rows(id1_vol_ts_pos |> mutate(direction = "Short"),
            id1_vol_ts_neg |> mutate(direction = "Long"))

breaks_jan <- seq.Date(as.Date("2021-01-01"), as.Date("2024-01-01"), "year")
breaks_jun <- seq.Date(as.Date("2021-07-01"), as.Date("2024-07-01"), "year")
datetime_breaks <- sort(c(breaks_jan, breaks_jun))
datetime_breaks <- as.POSIXct(datetime_breaks, tz="CET")

id1_vol_ma_plot <- plot_data |> 
  ggplot(aes(x = delivery_start, 
             y = value,
             col = factor(direction, levels = c("Short", "Long")))) +
  geom_line() +
  scale_x_datetime(
    breaks = datetime_breaks,
    labels = scales::date_format("%Y-%m")
  ) +
  scale_y_continuous(
    breaks = seq(0, 155, 25),
    labels = seq(0, 150, 25)
  ) +
  scale_color_manual(
    name = "",
    values = c("Short" = "#E76F51", 
               "Long" = "#2A9D8F")
  ) +
  labs(x = "Time", y = "One-month moving average of ID1 traded volumes (MW)") +
  theme_bw() +
  theme_custom() +
  theme(legend.position = "bottom",
        legend.margin = margin(1, 3, 3, 3),
        legend.text = element_text(size = 10))

ggsave(file.path(output_path, "figures/id1_vol_ma_plot.pdf"),
       id1_vol_ma_plot, width = 8, height = 6, 
       units = "in", dpi = 300, useDingbats = TRUE)
