library(dplyr)
library(tidyverse)
library(readr)
library(here)
library(cregg)
library(ggplot2)
library(patchwork)
library(grid)
library(Matrix)
library(viridis)

source("functions/r-assist.R")

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

main_text_size <- 10

mm_heat <- cj(
  df_heat,
  Y ~ year + tax + ban + heatpump + energyclass + exemption,
  id = ~id,
  estimate = "mm",
  by = ~justice_class
)

mm_pv <- cj(
  df_pv,
  Y ~ mix + imports + pv + tradeoffs + distribution,
  id = ~id,
  estimate = "mm",
  by = ~justice_class
)

plot_heat <- plot(
  mm_heat,
  group = "justice_class"
) +
  geom_vline(xintercept = 0.5, color = "grey50", alpha = 0.8) +
  xlim(0.3, 0.7) +
  scale_color_viridis_d(end = .8, na.translate = FALSE) +
  theme_classic() +
  theme(legend.title = element_blank()) +
  ggtitle("Heating sector decarbonisation")

plot_pv <- plot(
  mm_pv,
  group = "justice_class"
) +
  geom_vline(xintercept = 0.5, color = "grey50", alpha = 0.8) +
  xlim(0.3, 0.7) +
  scale_color_viridis_d(end = .8, na.translate = FALSE) +
  theme_classic() +
  theme(legend.title = element_blank()) +
  ggtitle("Renewable energy scale-up")

plot <- plot_heat + plot_pv + plot_layout(guides = "collect")

ggsave(
  plot = plot,
  here("output", "mm_justice.png"),
  height = 6, width = 10
)
