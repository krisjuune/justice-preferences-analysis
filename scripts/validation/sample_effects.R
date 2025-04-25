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


###################### region effect #########################

pv_amce_all <- cj(
  df_pv,
  Y ~ mix + imports + pv + tradeoffs + distribution,
  id = ~id,
  estimate = "amce"
) |> mutate(group = "All respondents")

pv_amce_noital <- cj(
  df_pv |> filter(!region %in% c("italian")),
  Y ~ mix + imports + pv + tradeoffs + distribution,
  id = ~id,
  estimate = "amce"
) |> mutate(group = "Without Italian subsample")

heat_amce_all <- cj(
  df_heat,
  Y ~ year + tax + ban + heatpump + energyclass + exemption,
  id = ~id,
  estimate = "amce"
) |> mutate(group = "All respondents")

heat_amce_noital <- cj(
  df_heat |> filter(!region %in% c("italian")),
  Y ~ year + tax + ban + heatpump + energyclass + exemption,
  id = ~id,
  estimate = "amce"
) |> mutate(group = "Without Italian subsample")

combined_heat <- bind_rows(heat_amce_all, heat_amce_noital)
combined_heat$group <- as.factor(combined_heat$group)

plot_heat <- plot(
  combined_heat,
  group = "group"
) +
  scale_color_manual(
    values = c("All respondents" = "black", "Without Italian subsample" = "gray")
  ) +
  theme_classic() +
  theme(legend.title = element_blank()) +
  ggtitle("Heating sector decarbonisation")

combined_pv <- bind_rows(pv_amce_all, pv_amce_noital)
combined_pv$group <- as.factor(combined_pv$group)

plot_pv <- plot(
  combined_pv,
  group = "group"
) +
  scale_color_manual(
    values = c("All respondents" = "black", "Without Italian subsample" = "gray")
  )  +
  theme_classic() +
  theme(legend.title = element_blank()) +
  ggtitle("Renewable energy scale-up")

plot <- plot_heat + plot_pv + plot_layout(guides = "collect")

ggsave(
  plot = plot,
  here("output", "amce_sample_effect_region.png"),
  height = 5, width = 10
)


###################### education effect #########################

pv_amce_all <- cj(
  df_pv,
  Y ~ mix + imports + pv + tradeoffs + distribution,
  id = ~id,
  estimate = "amce"
) |> mutate(edu = "All respondents")

heat_amce_all <- cj(
  df_heat,
  Y ~ year + tax + ban + heatpump + energyclass + exemption,
  id = ~id,
  estimate = "amce"
) |> mutate(edu = "All respondents")

pv_amce_lowedu <- cj(
  df_pv |> filter(!education %in% c("no secondary")),
  Y ~ mix + imports + pv + tradeoffs + distribution,
  id = ~id,
  estimate = "amce"
) |> mutate(edu = "Without lower education group")

heat_amce_lowedu <- cj(
  df_heat |> filter(!education %in% c("no secondary")),
  Y ~ year + tax + ban + heatpump + energyclass + exemption,
  id = ~id,
  estimate = "amce"
) |> mutate(edu = "Without lower education group")

combined_heat <- bind_rows(heat_amce_all, heat_amce_lowedu)
combined_heat$edu <- as.factor(combined_heat$edu)

plot_heat <- plot(
  combined_heat,
  group = "edu"
) +
  scale_color_manual(
    values = c(
      "All respondents" = "black",
      "Without lower education group" = "gray"
    ),
    na.translate = FALSE
  ) +
  theme_classic() +
  theme(legend.title = element_blank()) +
  ggtitle("Heating sector decarbonisation")

combined_pv <- bind_rows(pv_amce_all, pv_amce_lowedu)
combined_pv$edu <- as.factor(combined_pv$edu)

plot_pv <- plot(
  combined_pv,
  group = "edu"
) +
  scale_color_manual(
    values = c(
      "All respondents" = "black",
      "Without lower education group" = "gray"
    ),
    na.translate = FALSE
  )  +
  theme_classic() +
  theme(legend.title = element_blank()) +
  ggtitle("Renewable energy scale-up")

plot_edu <- plot_heat + plot_pv + plot_layout(guides = "collect")

ggsave(
  plot = plot_edu,
  here("output", "amce_sample_effect_education.png"),
  height = 5, width = 10
)
