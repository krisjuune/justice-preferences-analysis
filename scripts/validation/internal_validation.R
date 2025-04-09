library(tidyverse)
library(dplyr)
library(purrr)
library(forcats)
library(arrow)
library(broom)
library(cregg)
library(survey)
library(marginaleffects)
library(broom.helpers)
library(parameters)
library(gridExtra)
library(grid)

source("functions/r-assist.R")

df_heat <- read.csv("data/heat-conjoint.csv")
df_pv <- read.csv("data/pv-conjoint.csv")

# remove speeders, laggards, and inattentives
df_heat <- filter_respondents(df_heat)
df_pv <- filter_respondents(df_pv)

# factorise conjoints and variables for subgroup analysis
df_heat <- factor_conjoint(df_heat, experiment = "heat")
df_pv <- factor_conjoint(df_pv, experiment = "pv")

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