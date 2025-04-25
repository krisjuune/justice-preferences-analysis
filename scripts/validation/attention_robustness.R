library(tidyverse)
library(dplyr)
library(cregg)
library(survey)
library(here)
library(cregg)
library(stringr)
library(forcats)
library(ggplot2)
library(patchwork)

source(here("functions", "r-assist.R"))

df_heat <- read_csv(
  here("data", "heat_conjoint.csv"),
  show_col_types = FALSE
) |>
  filter_respondents() |>
  factor_conjoint(experiment = "heat")

df_pv <- read_csv(
  here("data", "pv_conjoint.csv"),
  show_col_types = FALSE
) |>
  filter_respondents() |>
  factor_conjoint(experiment = "pv")

# calculate nr of observations where non-chosen package rated higher
dodgy_pct_heat <- df_heat |>
  group_by(ID, task_num) |>
  summarise(non_chosen_better = {
    chosen_rating <- rating[Y == 1]
    non_chosen_rating <- rating[Y == 0]
    non_chosen_rating > chosen_rating
  }, .groups = "drop") |>
  summarise(percentage = mean(non_chosen_better) * 100)

dodgy_pct_pv <- df_pv |>
  group_by(ID, task_num) |>
  summarise(non_chosen_better = {
    chosen_rating <- rating[Y == 1]
    non_chosen_rating <- rating[Y == 0]
    non_chosen_rating > chosen_rating
  }, .groups = "drop") |>
  summarise(percentage = mean(non_chosen_better) * 100)

################ rerun without dodgy #####################

dodgy_ids_heat <- df_heat |>
  group_by(ID, task_num) |>
  summarise(non_chosen_better = {
    chosen_rating <- rating[Y == 1]
    non_chosen_rating <- rating[Y == 0]
    any(non_chosen_rating > chosen_rating)
  }, .groups = "drop") |>
  filter(non_chosen_better) |>
  distinct(ID) |>
  pull(ID)

dodgy_ids_pv <- df_pv |>
  group_by(ID, task_num) |>
  summarise(non_chosen_better = {
    chosen_rating <- rating[Y == 1]
    non_chosen_rating <- rating[Y == 0]
    any(non_chosen_rating > chosen_rating)
  }, .groups = "drop") |>
  filter(non_chosen_better) |>
  distinct(ID) |>
  pull(ID)

df_heat <- df_heat |>
  mutate(dodgy_flag = ID %in% dodgy_ids_heat)
df_pv <- df_pv |>
  mutate(dodgy_flag = ID %in% dodgy_ids_pv)

pv_mm_all <- cj(
  df_pv,
  Y ~ mix + imports + pv + tradeoffs + distribution,
  id = ~id,
  estimate = "amce"
) |> mutate(group = "All")

pv_mm_clean <- cj(
  df_pv |> filter(!dodgy_flag),
  Y ~ mix + imports + pv + tradeoffs + distribution,
  id = ~id,
  estimate = "amce"
) |> mutate(group = "Clean")

heat_mm_all <- cj(
  df_heat,
  Y ~ year + tax + ban + heatpump + energyclass + exemption,
  id = ~id,
  estimate = "amce"
) |> mutate(group = "All")

heat_mm_clean <- cj(
  df_heat |> filter(!dodgy_flag),
  Y ~ year + tax + ban + heatpump + energyclass + exemption,
  id = ~id,
  estimate = "amce"
) |> mutate(group = "Clean")

###################### make plots #########################


combined_heat <- bind_rows(heat_mm_all, heat_mm_clean)
combined_heat$group <- as.factor(combined_heat$group)

plot_heat <- plot(
  combined_heat,
  group = "group"
) +
  scale_color_manual(
    values = c("All" = "black", "Clean" = "gray")
  ) +
  theme_classic() +
  theme(legend.title = element_blank()) +
  ggtitle("Heating sector decarbonisation")

combined_pv <- bind_rows(pv_mm_all, pv_mm_clean)
combined_pv$group <- as.factor(combined_pv$group)

plot_pv <- plot(
  combined_pv,
  group = "group"
) +
  scale_color_manual(
    values = c("All" = "black", "Clean" = "gray")
  )  +
  theme_classic() +
  theme(legend.title = element_blank()) +
  ggtitle("Renewable energy scale-up")

plot <- plot_heat + plot_pv + plot_layout(guides = "collect")

ggsave(
  plot = plot,
  here("output", "amce_attention_robustness.png"),
  height = 6, width = 10
)
