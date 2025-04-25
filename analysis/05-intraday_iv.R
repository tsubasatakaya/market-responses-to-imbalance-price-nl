source("src/R/setup.R")
source("src/R/functions.R")

dv <- "id1_vol_quarter_hourly"

#-------------------------------------------------------------------------------
# Data preparation
#-------------------------------------------------------------------------------
imb_data <- get_processed_base_data()
pos_data <- imb_data |> 
  filter(regulation_state_pos_dummy == 1)
neg_data <- imb_data |> 
  filter(regulation_state_pos_dummy == 0)

#-------------------------------------------------------------------------------
# Modelsummary setup
#-------------------------------------------------------------------------------
# Coef map
cm <- c("fit_imbalance_price" = "Imbalance settlement price",
        "(Intercept)" = "Constant")
# Gof map
gof_f <- function(x) format(round(x, 2), big.mark = ",")
gm <- list(
  list("raw" = "nobs", "clean" = "Observations", "fmt" = gof_f)
)

#-------------------------------------------------------------------------------
# Fit 2SLS by year
#-------------------------------------------------------------------------------
iv_model_tags <- c("pos", "neg")
iv_linear_res <- list()
data_list <- list(pos_data, neg_data, imb_data)

iv_linear_res_by_year <- list()
data_list <- list(pos_data, neg_data)
direc_by_year <- c()
years <- 2021:2024

model_count <- 0
for (year in years) {
  for (i in seq_along(iv_model_tags)) {
    model_tag <- iv_model_tags[i]
    
    res <- fit_iv_linear(data_list[[i]] |> filter(year == {{year}}),
                         dv = dv,
                         endo = endo,
                         cov = cov,
                         iv = iv_set[[model_tag]],
                         vcov = vcov_NW(time = ~ delivery_start)
    )
    iv_linear_res_by_year[[paste0("(", i + model_count, ")")]] <- res
    direc_by_year <- c(direc_by_year, iv_model_tags[i])
  }
  model_count <- model_count + i
}

# Extract F-statistics from model objects
f_stats_yearly <- map_dbl(iv_linear_res_by_year, 
                          ~ round(fitstat(.x, "ivf")[["ivf1::imbalance_price"]][["stat"]], 
                                  2))

#-------------------------------------------------------------------------------
# Summary table
#-------------------------------------------------------------------------------
add_rows <- tibble(term = "First stage F-statistic")
for (i in seq_along(f_stats_yearly)) {
  add_rows[paste0("col", i)] = f_stats_yearly[i]
}
attr(add_rows, "position") <- 5

modelsummary(iv_linear_res_by_year, 
             fmt = NULL,
             estimate = "{round(estimate, 3)}",
             statistic = "({round(std.error, 3)})",
             coef_map = cm, gof_map = gm,
             add_rows = add_rows,
             output = "gt") |> 
  tab_spanner(label = "Short system", columns = c(2, 4, 6, 8), gather = FALSE) |>
  tab_spanner(label = "Long system", columns = c(3, 5, 7, 9), gather = FALSE) |> 
  tab_spanner(label = "2021", columns = 2:3) |> 
  tab_spanner(label = "2022", columns = 4:5) |> 
  tab_spanner(label = "2023", columns = 6:7) |> 
  tab_spanner(label = "2024", columns = 8:9) |> 
  rows_add(
    starts_with(" ") ~ "Instrument",
    c(`(1)`, `(3)`, `(5)`, `(7)`) ~ "Z_up",
    c(`(2)`, `(4)`, `(6)`, `(8)`) ~ "Z_down",
    .before = 5
  ) |> 
  tab_options(table.font.size = "9pt",
              table.width  = pct(100),
              latex.use_longtable = TRUE) |> 
  gtsave("iv_linear_id1_qh_vol_by_year_summary.tex", path = file.path(output_path, "tables"))


#-------------------------------------------------------------------------------
# Plot coefficients
#-------------------------------------------------------------------------------
coef_by_year <- tibble()
for (i in seq_along(iv_linear_res_by_year)) {
  df <- tidy(iv_linear_res_by_year[[i]], conf.int = TRUE) |> 
    filter(str_detect(term, paste0("fit_", endo))) |> 
    mutate(direction = direc_by_year[i],
           year = rep(years, each = 2)[i])
  coef_by_year <- coef_by_year |> 
    bind_rows(df)
}

coef_plot_by_year <- coef_by_year |> 
  mutate(direction = factor(direction, levels = c("pos", "neg"))) |> 
  ggplot(aes(x = year, ymin = conf.low, ymax = conf.high)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + 
  geom_linerange(aes(col = direction), linewidth = 1, 
                 position = position_dodge(width = 0.2)) +
  geom_point(aes(x = year, y = estimate, col = direction), size = 3,
             position = position_dodge(width = 0.2)) + 
  scale_color_manual(name = "",
                     values = c("pos" = "#E76F51",
                                "neg" = "#2A9D8F"),
                     labels = c("Short", "Long")) +
  labs(x = "",
       y = "Effect of imbalance price on traded volumes (MW per €/MWh)") +
  theme_bw() +
  theme_custom() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.margin = margin(1, 3, 3, 3),
    axis.title = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 12),
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave(file.path(output_path, "figures/iv_linear_id1_qh_vol_coef_by_year.pdf"),
       coef_plot_by_year, width = 8, height = 6, 
       units = "in", dpi = 150, useDingbats = TRUE)







