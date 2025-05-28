library(tidyverse)
library(dplyr)
library(survey)
library(here)
library(readr)
library(cregg)
source("functions/r-assist.R")

df_heat <- read_csv(here("data", "heat_conjoint.csv"))
df_pv <- read_csv(here("data", "pv_conjoint.csv"))

# clean
df_heat <- filter_respondents(df_heat)
df_pv <- filter_respondents(df_pv)

# factorise conjoints and variables for subgroup analysis
df_heat <- factor_conjoint(df_heat, experiment = "heat")
df_pv <- factor_conjoint(df_pv, experiment = "pv")

# add column push
df_heat <- df_heat |>
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

df_pv <- df_pv |>
  mutate(push = case_when(
    pv %in% c(
      "All new and existing buildings",
      "New and existing non-residential buildings"
    ) ~ "strong",
    pv %in% c(
      "New non-residential buildings",
      "All new buildings"
    ) ~ "soft",
    pv == "No rooftop PV obligation" ~ "none",
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
mm_heat_push_rating <- cj(
  df_heat,
  rating ~ push,
  id = ~id,
  estimate = "mm",
  by = ~justice_class
)

mm_heat_push_choice <- cj(
  df_heat,
  Y ~ push,
  id = ~id,
  estimate = "mm",
  by = ~justice_class
)

mm_pv_push_rating <- cj(
  df_pv,
  rating ~ push,
  id = ~id,
  estimate = "mm",
  by = ~justice_class
)

mm_pv_push_choice <- cj(
  df_pv,
  Y ~ push,
  id = ~id,
  estimate = "mm",
  by = ~justice_class
)

mm_heat_push_rating_gen <- cj(
  df_heat,
  rating ~ push,
  id = ~id,
  estimate = "mm"
)

mm_heat_push_choice_gen <- cj(
  df_heat,
  Y ~ push,
  id = ~id,
  estimate = "mm"
)

mm_pv_push_rating_gen <- cj(
  df_pv,
  rating ~ push,
  id = ~id,
  estimate = "mm"
)

mm_pv_push_choice_gen <- cj(
  df_pv,
  Y ~ push,
  id = ~id,
  estimate = "mm"
)

# combine and save to file
mm_heat_push_choice <- mm_heat_push_choice |>
  select(-BY) |>
  mutate(outcome = "choice") |>
  mutate(experiment = "heat")

mm_heat_push_rating <- mm_heat_push_rating |>
  select(-BY) |>
  mutate(outcome = "rating") |>
  mutate(experiment = "heat")

mm_pv_push_choice <- mm_pv_push_choice |>
  select(-BY) |>
  mutate(outcome = "choice") |>
  mutate(experiment = "pv")

mm_pv_push_rating <- mm_pv_push_rating |>
  select(-BY) |>
  mutate(outcome = "rating") |>
  mutate(experiment = "pv")

mm_push_combined <- bind_rows(
  mm_heat_push_choice,
  mm_heat_push_rating,
  mm_pv_push_choice,
  mm_pv_push_rating
)

mm_heat_push_choice_gen <- mm_heat_push_choice_gen |>
  mutate(outcome = "choice") |>
  mutate(experiment = "heat")

mm_heat_push_rating_gen <- mm_heat_push_ratin_geng |>
  mutate(outcome = "rating") |>
  mutate(experiment = "heat")

mm_pv_push_choice_gen <- mm_pv_push_choice_gen |>
  mutate(outcome = "choice") |>
  mutate(experiment = "pv")

mm_pv_push_rating_gen <- mm_pv_push_rating_gen |>
  mutate(outcome = "rating") |>
  mutate(experiment = "pv")

mm_push_combined_gen <- bind_rows(
  mm_heat_push_choice_gen,
  mm_heat_push_rating_gen,
  mm_pv_push_choice_gen,
  mm_pv_push_rating_gen
)

write_csv(mm_push_combined, here("data", "mm_stringency.csv"))
write_csv(mm_push_combined_gen, here("data", "mm_stringency_overall.csv"))
