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

# add column utilitarian
df_heat <- df_heat |>
  mutate(util_pack = case_when(
    tax %in% c("100%", "75%") &
      ban == "No ban" &
      exemption == "No exemptions"
    ~ "utilitarian",
    exemption %in% c(
      "Low-income exempted",
      "Low- and middle-income exempted"
    ) &
      ban %in% c(
        "Ban new installations",
        "Ban and replace fossil heating"
      ) &
      tax %in% c("25%", "0%")
    ~ "egalitarian",
    TRUE ~ "other"
  )
  ) |>
  mutate(
    util_pack = factor(
      util_pack,
      levels = c(
        "utilitarian",
        "egalitarian",
        "other"
      ),
      labels = c(
        "Utilitarian packages",
        "Egalitarian packages",
        "Other packages"
      )
    )
  )

df_pv <- df_pv |>
  mutate(util_pack = case_when(
    pv == "No rooftop PV obligation" &
      tradeoffs != "No biodiversity trade-offs" &
      distribution == "No agreed cantonal production requirements"
    ~ "utilitarian",
    pv != "No rooftop PV obligation" &
      tradeoffs == "No biodiversity trade-offs" &
      distribution != "No agreed cantonal production requirements"
    ~ "egalitarian",
    TRUE ~ "other"
  )
  ) |>
  mutate(
    util_pack = factor(
      util_pack,
      levels = c(
        "utilitarian",
        "egalitarian",
        "other"
      ),
      labels = c(
        "Utilitarian packages",
        "Egalitarian packages",
        "Other packages"
      )
    )
  )

df_heat |>
  count(util_pack)
df_pv |>
  count(util_pack)

# util packages are about 4-5% of total packages

# get MMs
mm_heat_util_pack_rating <- cj(
  df_heat,
  rating ~ util_pack,
  id = ~id,
  estimate = "mm",
  by = ~justice_class
)

mm_heat_util_pack_choice <- cj(
  df_heat,
  Y ~ util_pack,
  id = ~id,
  estimate = "mm",
  by = ~justice_class
)

mm_pv_util_pack_rating <- cj(
  df_pv,
  rating ~ util_pack,
  id = ~id,
  estimate = "mm",
  by = ~justice_class
)

mm_pv_util_pack_choice <- cj(
  df_pv,
  Y ~ util_pack,
  id = ~id,
  estimate = "mm",
  by = ~justice_class
)

mm_heat_util_pack_rating_gen <- cj(
  df_heat,
  rating ~ util_pack,
  id = ~id,
  estimate = "mm"
)

mm_heat_util_pack_choice_gen <- cj(
  df_heat,
  Y ~ util_pack,
  id = ~id,
  estimate = "mm"
)

mm_pv_util_pack_rating_gen <- cj(
  df_pv,
  rating ~ util_pack,
  id = ~id,
  estimate = "mm"
)

mm_pv_util_pack_choice_gen <- cj(
  df_pv,
  Y ~ util_pack,
  id = ~id,
  estimate = "mm"
)

# combine and save to file
mm_heat_util_pack_choice <- mm_heat_util_pack_choice |>
  select(-BY) |>
  mutate(outcome = "choice") |>
  mutate(experiment = "heat")

mm_heat_util_pack_rating <- mm_heat_util_pack_rating |>
  select(-BY) |>
  mutate(outcome = "rating") |>
  mutate(experiment = "heat")

mm_pv_util_pack_choice <- mm_pv_util_pack_choice |>
  select(-BY) |>
  mutate(outcome = "choice") |>
  mutate(experiment = "pv")

mm_pv_util_pack_rating <- mm_pv_util_pack_rating |>
  select(-BY) |>
  mutate(outcome = "rating") |>
  mutate(experiment = "pv")

mm_util_pack_combined <- bind_rows(
  mm_heat_util_pack_choice,
  mm_heat_util_pack_rating,
  mm_pv_util_pack_choice,
  mm_pv_util_pack_rating
)

mm_heat_util_pack_choice_gen <- mm_heat_util_pack_choice_gen |>
  mutate(outcome = "choice") |>
  mutate(experiment = "heat")

mm_heat_util_pack_rating_gen <- mm_heat_util_pack_rating_gen |>
  mutate(outcome = "rating") |>
  mutate(experiment = "heat")

mm_pv_util_pack_choice_gen <- mm_pv_util_pack_choice_gen |>
  mutate(outcome = "choice") |>
  mutate(experiment = "pv")

mm_pv_util_pack_rating_gen <- mm_pv_util_pack_rating_gen |>
  mutate(outcome = "rating") |>
  mutate(experiment = "pv")

mm_util_pack_combined_gen <- bind_rows(
  mm_heat_util_pack_choice_gen,
  mm_heat_util_pack_rating_gen,
  mm_pv_util_pack_choice_gen,
  mm_pv_util_pack_rating_gen
)

write_csv(mm_util_pack_combined, here("data", "mm_instrument.csv"))
write_csv(mm_util_pack_combined_gen, here("data", "mm_instrument_overall.csv"))
