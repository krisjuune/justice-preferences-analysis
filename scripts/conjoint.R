library(tidyverse)
library(arrow)
library(broom)
library(cregg)
library(marginaleffects)
library(survey)
library(dplyr)


df_heat <- read.csv("data/heat-conjoint.csv")
df_pv <- read.csv("data/pv-conjoint.csv")

# effects sizes for pv experiment are surprisingly small, on the order of
# 0.03 compared to 0.1 to 0.2 in the heat experiment

#AMCE = regression coefficients where there’s an omitted reference category
#Marginal means = conditional averages for different category levels

########################### recode attribute levels #######################
df_heat <- df_heat %>%
  mutate(
    # rating = factor(
    #   case_when(
    #     rating == "0" ~ 0,
    #     rating == "1" ~ 0,
    #     rating == "2" ~ 0,
    #     rating == "3" ~ 1,
    #     rating == "4" ~ 1,
    #     rating == "5" ~ 1,
    #     TRUE ~ NA_real_),
    #   levels = 0:1
    # ),
    year = factor(
      case_when(
        year == "2050" ~ 0,
        year == "2045" ~ 1,
        year == "2040" ~ 2,
        year == "2035" ~ 3,
        year == "2030" ~ 4,
        TRUE ~ NA_real_
      ),
      levels = 0:4,
      labels = c("2050", "2045", "2040", "2035", "2030")
    ),
    tax = factor(
      case_when(
        tax == "0%" ~ 0,
        tax == "25%" ~ 1,
        tax == "50%" ~ 2,
        tax == "75%" ~ 3,
        tax == "100%" ~ 4,
        TRUE ~ NA_real_
      ),
      levels = 0:4,
      labels = c("0%", "25%", "50%", "75%", "100%")
    ),
    ban = factor(
      case_when(
        ban == "No ban" ~ 0,
        ban == "Ban on new installations" ~ 1,
        ban == "Ban and fossil heating replacement" ~ 2,
        TRUE ~ NA_real_
      ),
      levels = 0:2,
      labels = c("No ban",
                 "Ban new installations",
                 "Ban and replace fossil heating")
    ),
    heatpump = factor(
      case_when(
        heatpump == "Subsidy" ~ 0,
        heatpump == "Governmental lease" ~ 1,
        heatpump == "Subscription" ~ 2,
        TRUE ~ NA_real_
      ),
      levels = 0:2,
      labels = c("Subsidized heat pump",
                 "Leased heat pump",
                 "Heat pump subscription")
    ),
    energyclass = factor(
      case_when(
        energyclass == "New buildings must be energy efficient" ~ 0,
        energyclass == "New buildings must be energy efficient and produce renewable electricity on-site" ~ 1, # nolint: line_length_linter.
        energyclass == "All buildings need to be energy efficient" ~ 2,
        energyclass == "All buildings need to be energy efficient and produce renewable electricity on-site" ~ 3, # nolint: line_length_linter.
        TRUE ~ NA_real_
      ),
      levels = 0:3,
      labels = c("New buildings efficient",
                 "New buildings efficient and renewable",
                 "All buildings efficient",
                 "All buildings efficient and renewable")
    ),
    exemption = factor(
      case_when(
        exemption == "No exemptions" ~ 0,
        exemption == "Low-income households are exempted" ~ 1,
        exemption == "Low and middle-income households are exempted" ~ 2,
        TRUE ~ NA_real_
      ),
      levels = 0:2,
      labels = c("No exemptions",
                 "Low-income exempted",
                 "Low- and middle-income exempted")
    )
  )

df_pv <- df_pv %>%
  mutate(
    # rating = factor(
    #   case_when(
    #     rating == "0" ~ 0,
    #     rating == "1" ~ 0,
    #     rating == "2" ~ 0,
    #     rating == "3" ~ 1,
    #     rating == "4" ~ 1,
    #     rating == "5" ~ 1,
    #     TRUE ~ NA_real_),
    #   levels = 0:1
    # ),
    mix = factor(
      case_when(
        mix == "More hydro" ~ 0,
        mix == "More solar" ~ 1,
        mix == "More wind" ~ 2,
        TRUE ~ NA_real_
      ),
      levels = 0:2,
      labels = c("More hydro", "More solar", "More wind")
    ),
    imports = factor(
      case_when(
        imports == "0%" ~ 0,
        imports == "10%" ~ 1,
        imports == "20%" ~ 2,
        imports == "30%" ~ 3,
        TRUE ~ NA_real_
      ),
      levels = 0:3,
      labels = c("0%", "10%", "20%", "30%")
    ),
    pv = factor(
      case_when(
        pv == "No obligation" ~ 0,
        pv == "New public and commercial buildings" ~ 1,
        pv == "New and existing public and commercial buildings" ~ 2,
        pv == "All new buildings" ~ 3,
        pv == "All new and existing buildings" ~ 4,
        TRUE ~ NA_real_
      ),
      levels = 0:4,
      labels = c("No rooftop PV obligation",
                 "New non-residential buildings",
                 "New and existing non-residential buildings",
                 "All new buildings",
                 "All new and existing buildings")
    ),
    tradeoffs = factor(
      case_when(
        tradeoffs == "No trade-offs" ~ 0,
        tradeoffs == "Alpine regions" ~ 1,
        tradeoffs == "Agricultural areas" ~ 2,
        tradeoffs == "Forests" ~ 3,
        tradeoffs == "Rivers" ~ 4,
        tradeoffs == "Lakes" ~ 5,
        TRUE ~ NA_real_
      ),
      levels = 0:5,
      labels = c("No biodiversity trade-offs",
                 "Alpine regions",
                 "Agricultural regions",
                 "Forests",
                 "Rivers",
                 "Lakes")
    ),
    distribution = factor(
      case_when(
        distribution == "No agreed distribution" ~ 0,
        distribution == "Potential-based" ~ 1,
        distribution == "Equal per person" ~ 2,
        distribution == "Minimum limit" ~ 3,
        distribution == "Maximum limit" ~ 4,
        TRUE ~ NA_real_
      ),
      levels = 0:4,
      labels = c("No agreed cantonal production requirements",
                 "Maximum production potential",
                 "Equal per person",
                 "Minimum limit",
                 "Maximum limit")
    )
  )

# testing the cregg package
heat_amce_choice <- amce(
  df_heat,
  Y ~ year + tax + ban + heatpump + energyclass + exemption,
  id = ~ID
)

pv_amce_choice <- amce(
  df_pv,
  Y ~ mix + imports + pv + tradeoffs + distribution,
  id = ~ID
)

plot(heat_amce_choice)
plot(pv_amce_choice)

# the rating data has the missing data problem that was true 
# for the choices before, there some kind of error with 
# stacking in prep data, looks like the right rating isn't
# matched with the right choice
# the AMCEs look like the pv choice used to look like
heat_amce_rating <- amce(
  df_heat,
  rating ~ year + tax + ban + heatpump + energyclass + exemption,
  id = ~ID
)

pv_amce_rating <- amce(
  df_pv,
  rating ~ mix + imports + pv + tradeoffs + distribution,
  id = ~ID
)

plot(heat_amce_rating)
plot(pv_amce_rating)

#################################### IRR ####################################



############################## marginal means ###############################
lin_model_pv <- lm(
  Y ~ mix + imports + pv + tradeoffs + distribution,
  data = pv_choices
)

tidy(lin_model_pv)

svydesign_pv <- svydesign(
  ids = pv_choices$ID,
  weights = 1,
  data = pv_choices
)

model_pv <- svyglm(
  Y ~ mix + imports + pv + tradeoffs + distribution,
  design = svydesign_pv
)

mfx_pv <- model_pv %>%
  avg_slopes(newdata = "mean")

plot_data_manual <- model_pv %>%
  tidy_and_attach() %>%
  tidy_add_reference_rows() %>%
  tidy_add_estimate_to_reference_rows() %>%
  filter(term != "(Intercept)") %>%
  mutate(term_nice = str_remove(term, variable)) %>%
  left_join(variable_lookup, by = join_by(variable)) %>% # make variable lookup
  mutate(across(c(term_nice, variable_nice), ~fct_inorder(.))) # make lookup

ggplot(
  plot_data_manual,
  aes(x = estimate, y = term_nice, color = variable_nice)
) +
  geom_vline(xintercept = 0) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) +
  scale_x_continuous(labels = label_pp) +
  guides(color = "none") +
  labs(
    x = "Percentage point change in probability of candidate selection",
    y = NULL,
    title = "AMCEs plotted with tidy_add_reference_rows()"
  ) +
  # Automatically resize each facet height with ggforce::facet_col()
  facet_col(facets = "variable_nice", scales = "free_y", space = "free")
