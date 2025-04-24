source("src/R/setup.R")
source("src/R/functions.R")

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
# Fit models
#-------------------------------------------------------------------------------
iv_model_tags <- c("pos", "neg", "comb")
iv_linear_res <- list()
data_list <- list(pos_data, neg_data, imb_data)

# Loop over short, long, and comb
for (i in seq_along(iv_model_tags)) {
  model_tag <- iv_model_tags[i]
  if (model_tag == "comb") {
    cov_all <- c(cov, "regulation_state_pos_dummy")
  } else {
    cov_all <- cov
  }
  res <- fit_iv_linear(data_list[[i]],
                       dv = dv,
                       endo = endo,
                       cov = cov_all,
                       iv = iv_set[[model_tag]],
                       vcov = vcov_NW(time = ~ delivery_start))
  iv_linear_res[[paste0("(", i, ")")]] <- res
}

# Extract F-statistics from model objects
f_stats <- map_dbl(iv_linear_res, 
                   ~ round(fitstat(.x, "ivf")[["ivf1::imbalance_price"]][["stat"]], 
                           2))

#-------------------------------------------------------------------------------
# Summary table
#-------------------------------------------------------------------------------
add_rows <- tibble(term = "First-stage F-statistic",
                   col1 = f_stats[1],
                   col2 = f_stats[2],
                   col3 = f_stats[3])
attr(add_rows, "position") <- 5

modelsummary(iv_linear_res, fmt = 2,
             coef_map = cm, gof_map = gm,
             add_rows = add_rows,
             output = "gt") |> 
  tab_spanner(label = "Short system", columns = 2) |>
  tab_spanner(label = "Long system", columns = 3) |> 
  tab_spanner(label = "Combined", columns = 4) |> 
  rows_add(
    starts_with(" ") ~ "Instrument",
    contains("1") ~ "Z_up",
    contains("2") ~ "Z_down",
    contains("3") ~ "Z_up, Z_down",
    .before = 5
  ) |> 
  tab_options(table.font.size = "9pt",
              table.width  = pct(100),
              latex.use_longtable = TRUE) |> 
  cols_width(!starts_with(" ") ~ px(150)) |> 
  gtsave("iv_linear_summary.tex", path = file.path(output_path, "tables"))


#-------------------------------------------------------------------------------
# Visualize demand curve
#-------------------------------------------------------------------------------
# Example: short system
plot_data <- pos_data |> 
  mutate(
    # Regress system imbalance on covariates
    imb_resid = resid(lm(as.formula(paste0(dv, "~ +", paste0(cov, collapse = "+"))),
                         data = pos_data)),
    # Regress imbalance price on covariates
    isp_resid = resid(lm(as.formula(paste0(endo, "~ +", paste0(cov, collapse = "+"))),
                         data = pos_data)),
    # Get fitted values of residualized imbalance price by IV
    isp_resid_fitted = predict(lm(isp_resid ~ !!sym(iv_set[["pos"]]),
                                  data = pos_data)) 
  ) |> 
  select(imb_resid, isp_resid, isp_resid_fitted)

ols_coefs <- coef(lm(imb_resid ~ isp_resid, data = plot_data))
ols_line <- tibble(
  isp = seq(-550, 2300, 10),
  imb_pred = ols_coefs[1] + ols_coefs[2] * isp
)
iv_coefs <- coef(lm(imb_resid ~ isp_resid_fitted, data = plot_data))
demand_curve <- tibble(
  isp = seq(-20, 70, 0.5),
  imb_pred = iv_coefs[1] + iv_coefs[2] * isp
)

# OLS plot
panel_1 <- ggplot(plot_data, aes(x = imb_resid, y = isp_resid)) +
  geom_point(size = 1, color = "gray") +
  geom_line(data = ols_line, aes(x = imb_pred, y = isp),
            linewidth = 1) +
  xlim(-320, 400) +
  ylim(-600, 1200) +
  theme_bw() +
  labs(x = "Residualized system imbalance (MW)",
       y = "Residualized imbalance price (€/MWh)",
       title = "OLS") +
  annotate("text", 
           x = -320 + 70, 
           y = -600 + 0.7,
           label = "balance", color = "black", size = 4, hjust = 0) +
  annotate("segment", 
           x = -320 + 60, 
           xend = -320, 
           y = -600 + 0.7,
           yend = -600 + 0.7,
           arrow = arrow(length = unit(0.3, "cm")), 
           color = "black", linewidth = 1) +
  annotate("text", 
           x = 400 - 200, 
           y = -600 + 0.7,
           label = "shortage", color = "black", size = 4, hjust = 0) +
  annotate("segment", 
           x = 400 - 60, 
           xend = 400, 
           y = -600 + 0.7,
           yend = -600 + 0.7,
           arrow = arrow(length = unit(0.3, "cm")), 
           color = "black", linewidth = 1) +
  theme(
    plot.title = element_text(size = 15,
                              hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    plot.margin = margin(5, 10, 5, 3),
    axis.title.y = element_text(margin = margin(0,0,0,0)),
  )

# IV plot
panel_2 <- ggplot(plot_data, aes(x = imb_resid, y = isp_resid_fitted)) +
  geom_point(size = 1, color = "gray") +
  geom_line(data = demand_curve, aes(x = imb_pred, y = isp),
            color = "#377EB8", linewidth = 1) +
  theme_bw() +
  labs(x = "Residualized system imbalance (MW)",
       y = "Fitted values of residualized imbalance price (€/MWh)",
       title = "IV") +
  xlim(-320, 400) +
  annotate("text", 
           x = -320 + 70, 
           y = min(plot_data$isp_resid_fitted) + 0.3,
           label = "balance", color = "black", size = 4, hjust = 0) +
  annotate("segment", 
           x = -320 + 60, 
           xend = -320, 
           y = min(plot_data$isp_resid_fitted),
           yend = min(plot_data$isp_resid_fitted),
           arrow = arrow(length = unit(0.3, "cm")), 
           color = "black", linewidth = 1) +
  annotate("text", 
           x = 400 - 200, 
           y = min(plot_data$isp_resid_fitted) + 0.3,
           label = "shortage", color = "black", size = 4, hjust = 0) +
  annotate("segment", 
           x = 400 - 60, 
           xend = 400, 
           y = min(plot_data$isp_resid_fitted),
           yend = min(plot_data$isp_resid_fitted),
           arrow = arrow(length = unit(0.3, "cm")), 
           color = "black", linewidth = 1) +
  theme(
    plot.title = element_text(size = 14,
                              color = "#377EB8",
                              hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    plot.margin = margin(5, 3, 5, 10),
    axis.title.y = element_text(margin = margin(0,0,0,0))
  )
demand_plot_combined <- grid.arrange(panel_1, panel_2,  
                                     ncol = 2, 
                                     widths = c(1, 1),
                                     padding = unit(1, "cm"))

ggsave(file.path(output_path, "figures/demand_curve_example.pdf"),
       demand_plot_combined, width = 8.27, height = 4.5, 
       units = "in", dpi = 70, useDingbats = TRUE)


#-------------------------------------------------------------------------------
# Estimates by year
#-------------------------------------------------------------------------------
iv_linear_res_by_year <- list()
data_list <- list(pos_data, neg_data, imb_data)
direc_by_year <- c()
years <- 2021:2024

model_count <- 0
for (year in years) {
  for (i in seq_along(iv_model_tags)) {
    model_tag <- iv_model_tags[i]
    if (model_tag == "comb") {
      cov_all <- c(cov, "regulation_state_pos_dummy")
    } else {
      cov_all <- cov
    }
    res <- fit_iv_linear(data_list[[i]] |> filter(year == {{year}}),
                         dv = dv,
                         endo = endo,
                         cov = cov_all,
                         iv = iv_set[[model_tag]],
                         vcov = vcov_NW(time = ~ delivery_start)
    )
    iv_linear_res_by_year[[paste0("(", i + model_count, ")")]] <- res
    direc_by_year <- c(direc_by_year, iv_model_tags[i])
  }
  model_count <- model_count + i
}


# Summary table
f_stats_yearly <- map_dbl(iv_linear_res_by_year, 
                          ~ round(fitstat(.x, "ivf")[["ivf1::imbalance_price"]][["stat"]], 
                                  2))

add_rows <- tibble(term = "First stage F-statistic")
for (i in seq_along(f_stats_yearly)) {
  add_rows[paste0("col", i)] = f_stats_yearly[i]
}
attr(add_rows, "position") <- 5

modelsummary(iv_linear_res_by_year, 
             fmt = 2,
             coef_map = cm, gof_map = gm,
             add_rows = add_rows,
             output = "gt") |> 
  tab_spanner(label = "Short system", columns = c(2, 5, 8, 11), gather = FALSE) |>
  tab_spanner(label = "Long system", columns = c(3, 6, 9, 12), gather = FALSE) |> 
  tab_spanner(label = "Combined", columns = c(4, 7, 10, 13), gather = FALSE) |> 
  tab_spanner(label = "2021", columns = 2:4) |> 
  tab_spanner(label = "2022", columns = 5:7) |> 
  tab_spanner(label = "2023", columns = 8:10) |> 
  tab_spanner(label = "2024", columns = 11:13) |> 
  rows_add(
    starts_with(" ") ~ "Instrument",
    c(`(1)`, `(4)`, `(7)`, `(10)`) ~ "Z_up",
    c(`(2)`, `(5)`, `(8)`, `(11)`) ~ "Z_down",
    c(`(3)`, `(6)`, `(9)`, `(12)`) ~ "Z_up, Z_down",
    .before = 5
  ) |> 
  cols_width(!starts_with(" ") ~ px(60)) |> 
  tab_options(table.font.size = "9pt",
              table.width  = pct(100),
              latex.use_longtable = TRUE) |> 
  gtsave("iv_linear_by_year_summary.tex", path = file.path(output_path, "tables"))


# Coef plot
coef_by_year <- tibble()
for (i in seq_along(iv_linear_res_by_year)) {
  df <- tidy(iv_linear_res_by_year[[i]], conf.int = TRUE) |> 
    filter(str_detect(term, paste0("fit_", endo))) |> 
    mutate(direction = direc_by_year[i],
           year = rep(years, each = 3)[i])
  coef_by_year <- coef_by_year |> 
    bind_rows(df)
}

coef_plot_by_year <- coef_by_year |> 
  mutate(direction = factor(direction, levels = c("pos", "neg", "comb"))) |> 
  ggplot(aes(x = year, ymin = conf.low, ymax = conf.high)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + 
  geom_linerange(aes(col = direction), linewidth = 1, 
                 position = position_dodge(width = 0.2)) +
  geom_point(aes(x = year, y = estimate, col = direction), size = 3,
             position = position_dodge(width = 0.2)) + 
  scale_color_manual(name = "",
                     values = c("pos" = "#E76F51",
                                "neg" = "#2A9D8F",
                                "comb" = "#000000"),
                     labels = c("Short", "Long", "Combined")) +
  labs(x = "", y = "Effect of imbalance price on system imbalance (MW per €/MWh)") +
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
    strip.text.x = element_text(size = 12),
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave(file.path(output_path, "figures/iv_linear_coef_plot_by_year.pdf"),
       coef_plot_by_year, width = 8.6, height = 7, 
       units = "in", dpi = 150, useDingbats = TRUE)





