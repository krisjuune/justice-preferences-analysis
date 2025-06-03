library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(patchwork)
library(here)

source(here("scripts", "plots", "instrument_type.R"))
source(here("functions", "r-assist.R"))

df_push_just <- read_csv(
  here("data", "mm_stringency.csv"),
  show_col_types = FALSE
)

df_push_gen <- read_csv(
  here("data", "mm_stringency_overall.csv"),
  show_col_types = FALSE
)

df_push <- bind_rows(
  df_push_just |> mutate(subgroups = "justice"),
  df_push_gen |> mutate(subgroups = "general")
) |>
  filter(outcome == "choice") |>
  mutate(
    level = factor(
      level,
      levels = c("Low stringency", "Medium stringency", "High stringency")
    ),
    experiment = factor(
      experiment,
      levels = c("pv", "heat"),
      labels = c("Renewable energy", "Heating sector")
    ),
    sample = case_when(
      subgroups == "justice" ~ as.character(justice_class),
      subgroups == "general" ~ "Overall"
    ),
    sample = factor(
      sample,
      levels = c("Egalitarianists", "Universalists", "Utilitarianists", "Overall"),
      labels = c(
        "Egalitarianists (39.3%)",
        "Universalists (50.9%)",
        "Utilitarianists (9.8%)",
        "Overall sample"
      )
    )
  )

push_plot <- df_push |>
  plot_features_per_class() +
  scale_x_continuous(limits = c(.29, .71))

ggsave(
  plot = push_plot,
  here("output", "mm_stringency.png"),
  height = 3, width = 10
)
