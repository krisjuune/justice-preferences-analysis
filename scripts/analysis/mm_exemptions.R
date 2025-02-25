library(tidyverse)
library(dplyr)
library(survey)
library(here)
library(readr)
library(cregg)
source("functions/r-assist.R")

df_heat <- read_csv(here("data", "heat-conjoint.csv"))

# clean
df_heat <- filter_respondents(df_heat)

# factorise conjoints and variables for subgroup analysis
df_heat <- factor_conjoint(df_heat, experiment = "heat")

# get MMs
mm_exemption_rating <- cj(
  df_heat,
  rating ~ tax + ban,
  id = ~ID,
  estimate = "mm",
  by = ~exemption + justice_class
)

mm_exemption_choice <- cj(
  df_heat,
  Y ~ tax + ban,
  id = ~ID,
  estimate = "mm",
  by = ~exemption + justice_class
)

# combine and save to file
mm_exemption_choice <- mm_exemption_choice |> select(-BY)
mm_exemption_rating <- mm_exemption_rating |> select(-BY)

mm_exemption_combined <- bind_rows(
  mm_exemption_choice |> mutate(outcome = "choice"),
  mm_exemption_rating |> mutate(outcome = "rating")
)

write_csv(mm_exemption_combined, here("data", "mm_exemptions.csv"))
