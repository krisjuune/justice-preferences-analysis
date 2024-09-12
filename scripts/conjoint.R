library(tidyverse)
library(arrow)
library(broom)
library(cregg)
library(marginaleffects)
library(survey)
library(dplyr)
library(broom.helpers)
library(parameters)
source("functions/r-assist.R")


df_heat <- read.csv("data/heat-conjoint.csv")
df_pv <- read.csv("data/pv-conjoint.csv")

#AMCE = regression coefficients where there’s an omitted reference category
#Marginal means = conditional averages for different category levels

# remove speeders, laggards, and inattentives
df_heat <- filter_respondents(df_heat)
df_pv <- filter_respondents(df_pv)

# factorise conjoints and variables for subgroup analysis
df_heat <- factor_conjoint(df_heat, experiment = "heat")
df_pv <- factor_conjoint(df_pv, experiment = "pv") 
#TODO there's an issue with preserving justice class, get only NaN values
# works fine for heat but not for pv, super weird

# set default theme and font stuff
theme_set(theme_nice())
update_geom_defaults("text", list(family = "Jost-Regular", fontface = "plain"))
update_geom_defaults("label", list(family = "Jost-Regular", fontface = "plain"))


############################## AMCE ##################################
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

############################## marginal means ###############################
# write function that does this
lin_model_pv <- lm(
  Y ~ mix + imports + pv + tradeoffs + distribution,
  data = df_pv
)

tidy(lin_model_pv)

svydesign_pv <- svydesign(
  ids = df_pv$ID,
  weights = 1,
  data = df_pv
)

model_pv <- svyglm(
  Y ~ mix + imports + pv + tradeoffs + distribution,
  design = svydesign_pv
)

#TODO think about weights
mfx_pv <- model_pv %>%
  avg_slopes(newdata = "mean") 

#TODO save output to file, so that plots can be made in Python in the end


plot_data_manual <- model_pv %>%
  tidy_and_attach() %>%
  tidy_add_reference_rows() %>%
  tidy_add_estimate_to_reference_rows() %>%
  filter(term != "(Intercept)")

ggplot(
  plot_data_manual,
  aes(x = estimate, y = term)
) +
  geom_vline(xintercept = 0) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high))


# plot_data_manual <- model_pv %>%
#   tidy_and_attach() %>%
#   tidy_add_reference_rows() %>%
#   tidy_add_estimate_to_reference_rows() %>%
#   filter(term != "(Intercept)") %>%
#   mutate(term_nice = str_remove(term, variable)) %>%
#   left_join(variable_lookup, by = join_by(variable)) %>% # make variable lookup
#   mutate(across(c(term_nice, variable_nice), ~fct_inorder(.))) # make lookup
# 
# ggplot(
#   plot_data_manual,
#   aes(x = estimate, y = term_nice, color = variable_nice)
# ) +
#   geom_vline(xintercept = 0) +
#   geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) +
#   scale_x_continuous(labels = label_pp) +
#   guides(color = "none") +
#   labs(
#     x = "Percentage point change in probability of candidate selection",
#     y = NULL,
#     title = "AMCEs plotted with tidy_add_reference_rows()"
#   ) +
#   # Automatically resize each facet height with ggforce::facet_col()
#   facet_col(facets = "variable_nice", scales = "free_y", space = "free")

response_var_pv <- "Y"
predictors_pv <- c("mix", "imports", "pv", "tradeoffs", "distribution")
results_pv <- marginal_means(df_pv, response_var_pv, predictors_pv, output_file = "data/pv_MMs.csv")

response_var_heat <- "Y"
predictors_heat <- c("year", "tax", "ban", "heatpump", "energyclass", "exemption")
results_heat <- marginal_means(df_heat, response_var_heat, predictors_heat, output_file = "data/heat_MMs.csv")

############################### subgroup MMs ################################

#Choice outcome
mm_justice_heat <- cj(
  df_heat,
  Y ~ year + tax + ban + heatpump + energyclass + exemption,
  id = ~ID,
  estimate = "mm",
  by = ~justice_class
)

mm_justice_heat

#plot
plot(mm_justice_heat, group = "justice_class", vline = 0.5) +
  # theme_nice() +
  labs(title = "Choice outcome") +
  xlim(0.3, 0.7)

#Choice outcome
mm_justice_pv <- cj(
  df_pv,
  Y ~ mix + imports + pv + tradeoffs + distribution,
  id = ~ID,
  estimate = "mm",
  by = ~justice_class
)

mm_justice_heat

#plot
plot(mm_justice_pv, group = "justice_class", vline = 0.5) +
  # theme_nice() +
  labs(title = "Choice outcome") +
  xlim(0.3, 0.7)

#################################### IRR ####################################
# run this in Python after this script, so save everything to file

write.csv(heat_amce_choice, "data/heat-amce.csv", row.names = TRUE)
write.csv(pv_amce_choice, "data/pv-amce.csv", row.names = TRUE)

############################ justice and demo ################################
# get combined df for justice analysis
df_just <- bind_rows(
  df_heat %>% select(ID, gender, age, region, income, citizen, renting, party, justice_class)#,
  #df_pv %>% select(ID, gender, age, region, income, citizen, renting, party, trust, justice_class)
) %>%
  distinct(ID, .keep_all = TRUE) 

df_long <- df_just %>%
  select(gender, age, region, income, citizen, renting, party, justice_class) %>%
  pivot_longer(cols = gender:party, 
               names_to = "variable", 
               values_to = "category")

#TODO fix below
# Run multinomial logistic regression
# multinom_model <- multinom(justice_class ~ gender + age_category + language_region + income_category + tenant_status + citizenship, 
#                            data = your_data)
# 
# # Summarize the results
# summary(multinom_model)
# 
# # If you want to calculate p-values:
# z <- summary(multinom_model)$coefficients / summary(multinom_model)$standard.errors
# p_values <- (1 - pnorm(abs(z), 0, 1)) * 2
# 
# # View p-values
# p_values

# Create a contingency table for justice_class and gender
heatmap_data <- df_heat %>%
  group_by(justice_class, gender) %>%
  summarise(count = n()) %>%
  ungroup()

# Optional: Normalize counts to percentages by gender
heatmap_data <- heatmap_data %>%
  group_by(gender) %>%
  mutate(percentage = count / sum(count) * 100)

# Plot the heatmap (using percentage or count)
ggplot(heatmap_data, aes(x = gender, y = justice_class, fill = percentage)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") + 
  labs(title = "Heatmap of Justice Class by Gender",
       x = "Gender", 
       y = "Latent Profile", 
       fill = "Percentage") +
  theme_minimal()

# Plot heatmap with facets
ggplot(df_long, aes(x = justice_class, fill = category)) +
  geom_bar(position = "fill") +  # To normalize counts as a proportion
  facet_wrap(~ variable, scales = "free", ncol = 2) +  # Create facets for each variable
  labs(title = "Latent Profiles by Demographic Variables",
       x = "Latent Profile",
       y = "Proportion") +
  theme_minimal() +
  theme(legend.position = "bottom")
