library(tidyverse)
library(dplyr)
library(survey)
library(here)
library(readr)
library(cregg)
source("functions/r-assist.R")

df_heat <- read_csv(
  here("data", "heat_conjoint.csv"),
  show_col_types = FALSE
)

df_heat <- df_heat |>
  filter_respondents() |>
  factor_conjoint(experiment = "heat") |>
  mutate(push = case_when(
    ban == "Ban and replace fossil heating" |
      tax %in% c("100%", "75%", "50%") ~ "strong",
    ban == "Ban new installations" |
      tax %in% c("25%") ~ "soft",
    ban == "No ban" | tax == "0%" ~ "none",
    TRUE ~ NA_character_
  )
  ) |>
  mutate(
    push = factor(
      push,
      levels = c(
        "strong",
        "soft",
        "none"
      ),
      labels = c(
        "High stringency",
        "Medium stringency",
        "Low stringency"
      )
    )
  )

# get MMs
mm_exemption_rating <- cj(
  df_heat,
  rating ~ tax + ban,
  id = ~id,
  estimate = "mm",
  by = ~exemption + justice_class
)

mm_exemption_choice <- cj(
  df_heat,
  Y ~ tax + ban,
  id = ~id,
  estimate = "mm",
  by = ~exemption + justice_class
)

# get AMCEs per stringency level
amce_exemption_rating_str_justice <- cj(
  df_heat,
  rating ~ exemption,
  id = ~id,
  estimate = "amce",
  by = ~push + justice_class
)

amce_exemption_choice_str_justice <- cj(
  df_heat,
  Y ~ exemption,
  id = ~id,
  estimate = "amce",
  by = ~push + justice_class
)

amce_exemption_rating_general <- cj(
  df_heat,
  rating ~ exemption,
  id = ~id,
  estimate = "amce",
  by = ~push
)

amce_exemption_choice_general <- cj(
  df_heat,
  Y ~ exemption,
  id = ~id,
  estimate = "amce",
  by = ~push
)

# combine and save to file
mm_exemption_tax_ban <- bind_rows(
  mm_exemption_choice |>
    select(-BY) |>
    mutate(
      outcome = "choice",
      subgroups = "justice"
    ),
  mm_exemption_rating |> 
    select(-BY) |>
    mutate(
      outcome = "rating",
      subgroups = "justice"
    )
)

amce_exemption_combined <- bind_rows(
  amce_exemption_choice_str_justice |>
    select(-BY) |>
    mutate(
      outcome = "choice",
      subgroups = "justice"
    ),
  amce_exemption_rating_str_justice |>
    select(-BY) |>
    mutate(
      outcome = "rating",
      subgroups = "justice"
    ),
  amce_exemption_choice_general |>
    select(-BY) |>
    mutate(
      outcome = "choice",
      subgroups = "general"
    ),
  amce_exemption_rating_general |>
    select(-BY) |>
    mutate(
      outcome = "rating",
      subgroups = "general"
    )
)

write_csv(mm_exemption_tax_ban, here("data", "mm_exemptions_tax_ban.csv"))
write_csv(amce_exemption_combined, here("data", "amce_exemptions.csv"))
