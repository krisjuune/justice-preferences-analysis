library(tidyverse)
library(dplyr)
library(survey)
library(here)
library(readr)
library(cregg)
source("functions/r-assist.R")

df_pv <- read_csv(here("data", "pv-conjoint.csv"))

# clean
df_pv <- filter_respondents(df_pv)

# factorise conjoints and variables for subgroup analysis
df_pv <- factor_conjoint(df_pv, experiment = "pv")

df_pv_mix <- df_pv |>
  mutate(stromversorgung = case_when(
    imports == "10%" &
      mix %in% c("More solar", "More wind") &
      pv == "New non-residential buildings"
    ~ "stromversorgung",
    TRUE ~ "other"
  )
  ) |>
  mutate(
    stromversorgung = factor(
      stromversorgung,
      levels = c(
        "stromversorgung",
        "other"
      ),
      labels = c(
        "Stromversorgung packages",
        "Other packages"
      )
    )
  )

df_pv_tradeoffs <- df_pv |>
  mutate(stromversorgung = case_when(
    imports == "10%" &
      pv == "New non-residential buildings" &
      tradeoffs %in% c("No tradeoffs", "Forests", "Alpine regions")
    ~ "stromversorgung",
    TRUE ~ "other"
  )
  ) |>
  mutate(
    stromversorgung = factor(
      stromversorgung,
      levels = c(
        "stromversorgung",
        "other"
      ),
      labels = c(
        "Stromversorgung packages",
        "Other packages"
      )
    )
  )

# get support percentage for Stromversorgung packages
support_pct_mix <- df_pv_mix |>
  filter(stromversorgung == "Stromversorgung packages") |>
  summarize(support = mean(rating > 0) * 100) |>
  pull(support)

support_pct_tradeoffs <- df_pv_tradeoffs |>
  filter(stromversorgung == "Stromversorgung packages") |>
  summarize(support = mean(rating > 0) * 100) |>
  pull(support)

# get MMs
mm_validation_rating_mix <- cj(
  df_pv_mix,
  rating ~ stromversorgung,
  id = ~ID,
  estimate = "mm"
)

mm_validation_choice_mix <- cj(
  df_pv_mix,
  Y ~ stromversorgung,
  id = ~ID,
  estimate = "mm"
)

mm_validation_rating_tradeoffs <- cj(
  df_pv_tradeoffs,
  rating ~ stromversorgung,
  id = ~ID,
  estimate = "mm"
)

mm_validation_choice_tradeoffs <- cj(
  df_pv_tradeoffs,
  Y ~ stromversorgung,
  id = ~ID,
  estimate = "mm"
)

mm_validation_combined <- bind_rows(
  mm_validation_choice_mix |>
    mutate(outcome = "choice") |>
    mutate(characteristic = "targetmix") |>
    left_join(
      df_pv_mix |>
        count(stromversorgung, name = "n"),
      by = c("level" = "stromversorgung")
    ),
  mm_validation_rating_mix |>
    mutate(outcome = "rating") |>
    mutate(characteristic = "targetmix") |>
    left_join(
      df_pv_mix |>
        count(stromversorgung, name = "n"),
      by = c("level" = "stromversorgung")
    ),
  mm_validation_choice_tradeoffs |>
    mutate(outcome = "choice") |>
    mutate(characteristic = "tradeoffs") |>
    left_join(
      df_pv_tradeoffs |>
        count(stromversorgung, name = "n"),
      by = c("level" = "stromversorgung")
    ),
  mm_validation_rating_tradeoffs |>
    mutate(outcome = "rating") |>
    mutate(characteristic = "tradeoffs") |>
    left_join(
      df_pv_tradeoffs |>
        count(stromversorgung, name = "n"),
      by = c("level" = "stromversorgung")
    )
) |>
  mutate(
    support_pct = case_when(
      outcome == "choice" ~ NA_real_,
      level == "Other packages" ~ NA_real_,
      characteristic == "targetmix" ~ support_pct_mix,
      characteristic == "tradeoffs" ~ support_pct_tradeoffs
    )
  )

write_csv(mm_validation_combined, here("data", "mm_validation.csv"))

# another idea could be to look at how many percent
# of people chose these packages (or rated them
# bpositively and then maybe use bootstrapping to
# get uncertainty estimates)