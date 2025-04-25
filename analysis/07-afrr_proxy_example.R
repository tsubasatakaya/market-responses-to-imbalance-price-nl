source("src/R/setup.R")
source("src/R/functions.R")

source_data_path <- "data"

#-------------------------------------------------------------------------------
# Example merit order before and after change
#-------------------------------------------------------------------------------
mol_data <- vroom(file.path(source_data_path, "example_mol_data.csv"))
mol_data_longer <- mol_data |> 
  pivot_longer(cols = dplyr::starts_with("price"),
               names_to = "direction",
               values_to = "price") |> 
  mutate(direction = str_replace(direction, "price_", "")) |> 
  mutate(volume = case_when(direction == "up" ~ volume,
                            direction == "down" ~ - volume,),
         mol_type = case_when(delivery_start == min(delivery_start) ~ "before",
                              delivery_start == max(delivery_start) ~ "after"),
         mol_type = factor(mol_type, 
                           levels = c("before", "after"),
                           labels = c("Before 2022-09-01", "After 2022-09-01"))
  ) |> 
  filter(!is.na(price)) |> 
  arrange(delivery_start, direction, volume)

ex_mol_plot <- mol_data_longer |> 
  ggplot(aes(x = volume, y = price, 
             color = factor(direction, levels = c("up", "down")))) +
  geom_point(size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#9D9D9D") +
  facet_wrap(~ mol_type) +
  scale_color_manual(name = "",
                     values = c("#E76F51", "#2A9D8F"),
                     labels = c("Upward", "Downward")) +
  scale_x_continuous(breaks = seq(-600, 300, 150)) +
  labs(x = "Volume (MW)", y = "Price (€/MWh)") +
  theme_bw() +
  theme_custom() +
  theme(legend.position = "bottom",
        legend.margin = margin(1, 3, 3, 3),
        legend.text = element_text(size = 12),
        plot.margin = margin(5, 10, 10, 3),)

ggsave(file.path(output_path, "figures/afrr_mol_example_plot.pdf"),
       ex_mol_plot, width = 8.27, height = 5.5, 
       units = "in", dpi = 150, useDingbats = TRUE)


#---------------------------------------------------------------------------------
# Activation probability weights
#---------------------------------------------------------------------------------
weights_data <- vroom(file.path(output_path, "data", "afrr_activation_probability_data.csv"))

sparse_mol_data <- weights_data |> 
  # Select relevant probability columns
  select(volume, 
         up_discrete_activated_prob, down_discrete_activated_prob,
         up_discrete_activated_cum_prob, down_discrete_activated_cum_prob) |> 
  # Drop missing values
  drop_na(any_of(dplyr::ends_with("prob")))

sparse_mol_data <- sparse_mol_data |> 
  # Select activation probability columns
  select(volume, up_discrete_activated_prob, down_discrete_activated_prob) |> 
  # Pivot to long format
  pivot_longer(cols = !volume,
               names_to = "direction",
               values_to = "prob") |> 
  # Parse the column name to direction
  mutate(direction = str_split_i(direction, "_", 1)) |> 
  # Left join conditional probability data 
  left_join(sparse_mol_data |> 
              # Apply the same procedure as the activation probability columns
              select(volume, 
                     up_discrete_activated_cum_prob, 
                     down_discrete_activated_cum_prob) |> 
              pivot_longer(cols = !volume,
                           names_to = "direction",
                           values_to = "cum_prob") |>
              mutate(direction = str_split_i(direction, "_", 1)),
            by = c("volume", "direction")) |> 
  # Make direction as a factor (for plot)
  mutate(direction = factor(direction, 
                            levels = c("down", "up"))) |> 
  mutate(volume = case_when(direction == "down" ~ - volume,
                            .default = volume)) 

dense_mol_data <- weights_data |> 
  # Select relevant probability columns
  select(volume, 
         up_continuous_activated_prob, down_continuous_activated_prob,
         up_continuous_activated_cum_prob, down_continuous_activated_cum_prob) |> 
  # Drop missing values
  drop_na(any_of(dplyr::ends_with("prob"))) 

dense_mol_data <- dense_mol_data |> 
  # Select activation probability columns
  select(volume, up_continuous_activated_prob, down_continuous_activated_prob) |> 
  # Pivot to long format
  pivot_longer(cols = !volume,
               names_to = "direction",
               values_to = "prob") |> 
  # Parse the column name to direction
  mutate(direction = str_split_i(direction, "_", 1)) |> 
  # Left join conditional probability data 
  left_join(dense_mol_data |> 
              # Apply the same procedure as the activation probability columns
              select(volume, 
                     up_continuous_activated_cum_prob, 
                     down_continuous_activated_cum_prob) |> 
              pivot_longer(cols = !volume,
                           names_to = "direction",
                           values_to = "cum_prob") |>
              mutate(direction = str_split_i(direction, "_", 1)),
            by = c("volume", "direction")) |> 
  # Make direction as a factor (for plot)
  mutate(direction = factor(direction, 
                            levels = c("down", "up"))) |> 
  mutate(volume = case_when(direction == "down" ~ - volume,
                            .default = volume)) 

sparse_rel_freq_plot <- sparse_mol_data |> 
  ggplot(aes(x = factor(volume), y = prob, fill = direction)) +
  geom_bar(stat = "identity", color = "#e9ecef", linewidth = 1) +
  facet_wrap(~ direction,
             scale = "free_x",
             labeller = labeller(direction = c("down" = "Downward",
                                               "up" = "Upward"))) +
  scale_fill_manual(name = "",
                    values = c("#2A9D8F", "#E76F51"),
                    labels = c("Downward", "Upward")) +
  labs(x = "Volume (MW)", y = "Relative frequency") +
  theme_bw() +
  theme_custom() +
  theme(legend.position = "none",
        plot.margin = margin(5, 10, 10, 3),)

dense_rel_freq_plot <- dense_mol_data |> 
  filter(abs(volume) <= 600) |> 
  ggplot(aes(x = volume, y = prob, fill = direction)) +
  geom_bar(stat = "identity", color = "#e9ecef", linewidth = 0.2) +
  facet_wrap(~ direction,
             scale = "free_x",
             labeller = labeller(direction = c("down" = "Downward",
                                               "up" = "Upward"))) +
  scale_fill_manual(name = "",
                    values = c("#2A9D8F", "#E76F51"),
                    labels = c("Downward", "Upward")) +
  labs(x = "Volume (MW)", y = "Relative frequency") +
  theme_bw() +
  theme_custom() +
  theme(legend.position = "none",
        plot.margin = margin(5, 10, 10, 3),)

ggsave(file.path(output_path, "figures/sparse_relative_frequency_plot.pdf"),
       sparse_rel_freq_plot, width = 8.27, height = 4.5, 
       units = "in", dpi = 150, useDingbats = TRUE)
ggsave(file.path(output_path, "figures/dense_relative_frequency_plot.pdf"),
       dense_rel_freq_plot, width = 8.27, height = 4.5, 
       units = "in", dpi = 150, useDingbats = TRUE)


#---------------------------------------------------------------------------------
# Weighted aFRR energy prices
#---------------------------------------------------------------------------------
weighted_price_data <- mol_data_longer |> 
  left_join(sparse_mol_data |> select(volume, direction, prob, cum_prob),
            by = c("volume", "direction")) |> 
  rename(sparse_prob = "prob",
         sparse_cum_prob = "cum_prob") |> 
  left_join(dense_mol_data |> select(volume, direction, prob, cum_prob),
            by = c("volume", "direction")) |> 
  rename(dense_prob = "prob",
         dense_cum_prob = "cum_prob") |> 
  mutate(prob = case_when(mol_type == "Before 2022-09-01" ~ sparse_prob,
                          mol_type == "After 2022-09-01" ~ dense_prob),
         cum_prob = case_when(mol_type == "Before 2022-09-01" ~ sparse_cum_prob,
                              mol_type == "After 2022-09-01" ~ dense_cum_prob)
  ) |> 
  select(delivery_start, volume, direction, mol_type, price, prob, cum_prob) |> 
  mutate(weighted_price = case_when(volume == max(volume) ~ price * cum_prob,
                                    volume == min(volume) ~ price * cum_prob,
                                    .default = price * prob),
         .by = mol_type)

weighted_price_plot <- weighted_price_data |> 
  ggplot(aes(x = volume, y = weighted_price, 
             color = factor(direction, levels = c("up", "down")))) +
  geom_point(size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#9D9D9D") +
  facet_wrap(~ mol_type) +
  scale_color_manual(name = "",
                     values = c("#E76F51", "#2A9D8F"),
                     labels = c("Upward", "Downward")) +
  scale_x_continuous(breaks = seq(-600, 300, 150)) +
  labs(x = "Volume (MW)", y = "Price (€/MWh)") +
  theme_bw() +
  theme_custom() +
  theme(legend.position = "bottom",
        legend.margin = margin(1, 3, 3, 3),
        legend.text = element_text(size = 12),
        plot.margin = margin(5, 10, 10, 3),)

ggsave(file.path(output_path, "figures/afrr_weighted_price_plot.pdf"),
       weighted_price_plot, width = 8.27, height = 5.5, 
       units = "in", dpi = 150, useDingbats = TRUE)
