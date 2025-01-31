library(tidyverse)
library(dplyr)
library(survey)
library(here)
library(readr)
library(cregg)
source("functions/r-assist.R")

df_heat <- read_csv(here("data", "heat-conjoint.csv"))
df_pv <- read_csv(here("data", "pv-conjoint.csv"))

# clean
df_heat <- filter_respondents(df_heat)
df_pv <- filter_respondents(df_pv)

# factorise conjoints and variables for subgroup analysis
df_heat <- factor_conjoint(df_heat, experiment = "heat")
df_pv <- factor_conjoint(df_pv, experiment = "pv")

# levels_util_heat = {
#     "year": "2050",
#     "tax": "100%", 
#     "tax": "75%",
#     "ban": "No ban", 
#     "heatpump": "Subsidy",
#     "exemptions": "none"
# }

# levels_util_pv = {
#     "imports": "30%",
#     "imports": "20%",
#     "pv": "No obligation", 
#     "tradeoffs": ["Lakes", "Rivers", "Forests", "Agriculture", "Alpine"],
#     "distribution": "No agreed cantonal production requirements"
# }

# add column utilitarian
df_heat <- df_heat |>
  mutate(util_pack = case_when(
    tax %in% c("100%", "75%") &
    #   year == "2050" &
      ban == "No ban" &
      exemption == "No exemptions"
    ~ "utilitarian",
    TRUE ~ "non-utilitarian"
  )
  ) |>
  mutate(
    util_pack = factor(
      util_pack,
      levels = c(
        "utilitarian",
        "non-utilitarian"
      ),
      labels = c(
        "Utilitarian package",
        "Other package"
      )
    )
  )

df_pv <- df_pv |>
  mutate(util_pack = case_when(
    pv == "No rooftop PV obligation" &
      imports != "0%" &
      tradeoffs != "No tradeoffs" &
      distribution == "No agreed cantonal production requirements"
    ~ "utilitarian",
    TRUE ~ "non-utilitarian"
  )
  ) |>
  mutate(
    util_pack = factor(
      util_pack,
      levels = c(
        "utilitarian",
        "non-utilitarian"
      ),
      labels = c(
        "Utilitarian package",
        "Other package"
      )
    )
  )

df_heat |>
  count(util_pack)
df_pv |>
  count(util_pack)

# util packages are about 3-4% of total packages

# get MMs
mm_heat_util_pack_rating <- cj(
  df_heat,
  rating ~ util_pack,
  id = ~ID,
  estimate = "mm",
  by = ~justice_class
)

mm_heat_util_pack_choice <- cj(
  df_heat,
  Y ~ util_pack,
  id = ~ID,
  estimate = "mm",
  by = ~justice_class
)

mm_pv_util_pack_rating <- cj(
  df_pv,
  rating ~ util_pack,
  id = ~ID,
  estimate = "mm",
  by = ~justice_class
)

mm_pv_util_pack_choice <- cj(
  df_pv,
  Y ~ util_pack,
  id = ~ID,
  estimate = "mm",
  by = ~justice_class
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

write_csv(mm_util_pack_combined, here("data", "mm_util_packages.csv"))
