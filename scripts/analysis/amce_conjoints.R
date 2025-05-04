library(tidyverse)
library(dplyr)
library(purrr)
library(forcats)
library(arrow)
library(cregg)
library(parameters)
library(gridExtra)
library(grid)
library(ggplot2)
library(patchwork)
library(here)
library(nnet)

source(here("functions", "r-assist.R"))

df_heat <- read_csv(
  here("data", "heat_conjoint.csv"),
  show_col_types = FALSE
) |>
  filter_respondents() |>
  factor_conjoint(experiment = "heat")

df_pv <- read_csv(
  here("data", "pv_conjoint.csv"),
  show_col_types = FALSE
) |>
  filter_respondents() |>
  factor_conjoint(experiment = "pv")

############################## AMCE ##################################

heat_amce_choice <- amce(
  df_heat,
  Y ~ year + tax + ban + heatpump + energyclass + exemption,
  id = ~id
) |>
  mutate(
    type = "Choice",
    experiment = "heat"
  )

pv_amce_choice <- amce(
  df_pv,
  Y ~ mix + imports + pv + tradeoffs + distribution,
  id = ~id
) |>
  mutate(
    type = "Choice",
    experiment = "pv"
  )

heat_amce_rating <- amce(
  df_heat,
  rating ~ year + tax + ban + heatpump + energyclass + exemption,
  id = ~id
) |>
  mutate(
    type = "Rating",
    experiment = "heat"
  )

pv_amce_rating <- amce(
  df_pv,
  rating ~ mix + imports + pv + tradeoffs + distribution,
  id = ~id
) |>
  mutate(
    type = "Rating",
    experiment = "pv"
  )

amce_all <- bind_rows(
  heat_amce_choice,
  heat_amce_rating,
  pv_amce_choice,
  pv_amce_rating
)

plot_heat_choice <- plot(
  amce_all |> filter(experiment == "heat", type == "Choice"),
  group = "type"
) +
  scale_color_manual(values = c("Choice" = "black")) +
  theme_classic() +
  theme(
    legend.position = "none"
  ) +
  ggtitle("Choice")

plot_heat_rating <- plot(
  amce_all |> filter(experiment == "heat", type == "Rating"),
  group = "type"
) +
  scale_color_manual(values = c("Rating" = "black")) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  ggtitle("Rating")

plot_pv_choice <- plot(
  amce_all |> filter(experiment == "pv", type == "Choice"),
  group = "type"
) +
  scale_color_manual(values = c("Choice" = "black")) +
  theme_classic() +
  theme(
    legend.position = "none"
  ) +
  ggtitle("Choice")

plot_pv_rating <- plot(
  amce_all |> filter(experiment == "pv", type == "Rating"),
  group = "type"
) +
  scale_color_manual(values = c("Rating" = "black")) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  ggtitle("Rating")

plot_heat_combined <- plot_heat_choice + plot_heat_rating +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(
    title = "Heating Sector Decarbonisation",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.34)
    )
  )

plot_pv_combined <- plot_pv_choice + plot_pv_rating +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(
    title = "Renewable energy scale-up",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.372)
    )
  )

ggsave(
  plot = plot_heat_combined,
  here("output", "amce_outcomes_heat.png"),
  height = 6, width = 10
)

ggsave(
  plot = plot_pv_combined,
  here("output", "amce_outcomes_pv.png"),
  height = 6, width = 10
)



############################## MMs ###############################
# calculate mm-s
mm_pv <- cj(
  df_pv,
  Y ~ mix + imports + pv + tradeoffs + distribution,
  id = ~ID,
  estimate = "mm"
)

mm_heat <- cj(
  df_heat,
  Y ~ year + tax + ban + heatpump + energyclass + exemption,
  id = ~ID,
  estimate = "mm"
)

plot(mm_pv)
plot(mm_heat)

write_csv(mm_pv, here("data", "mm_pv.csv"))
write_csv(mm_heat, here("data", "mm_heat.csv"))

write.csv(heat_amce_choice, "data/heat_amce.csv", row.names = TRUE)
write.csv(pv_amce_choice, "data/pv_amce.csv", row.names = TRUE)




############################ justice and demo ################################
# get combined df for justice analysis
df_just <- bind_rows(
  df_heat %>% select(id, 
                     gender, 
                     age, 
                     region, 
                     income, 
                     education,
                     citizen, 
                     renting, 
                     urbanness,
                     party, 
                     trust,
                     justice_class),
  df_pv %>% select(id, 
                   gender, 
                   age, 
                   region,
                   income, 
                   education, 
                   citizen, 
                   renting, 
                   urbanness, 
                   party, 
                   trust, 
                   justice_class)
) %>%
  distinct(id, .keep_all = TRUE) 

df_long <- df_just %>%  # for plotting the group compositions
  select(gender, 
         age, 
         region, 
         income, 
         # education,
         # citizen, 
         # renting, 
         urbanness,
         party, 
         trust,
         justice_class) %>%
  pivot_longer(cols = gender:party, 
               names_to = "variable", 
               values_to = "category")

# Run multinomial logistic regression
df_just$justice_class <- relevel(df_just$justice_class, ref = "Universalists")
multinom_model <- multinom(justice_class ~ 
                             gender +
                             age +
                             region +
                             income +
                             education +
                             citizen +
                             renting +
                             urbanness  +
                             party +
                             trust,
                           data = df_just)

# Summarize the results
summary_model <- summary(multinom_model)

# Extract coefficients
coefficients <- summary_model$coefficients

# Extract standard errors
standard_errors <- summary_model$standard.errors

# Calculate z-values and p-values
z <- coefficients / standard_errors
p_values <- (1 - pnorm(abs(z), 0, 1)) * 2

# Calculate odds ratios
odds_ratios <- exp(coefficients)

# 95% CI for odds ratios
ci_lower <- exp(coefficients - 1.96 * standard_errors)
ci_upper <- exp(coefficients + 1.96 * standard_errors)

# Convert to data frames
coefficients_df <- as.data.frame(coefficients)
odds_ratios_df <- as.data.frame(odds_ratios)
ci_lower_df <- as.data.frame(ci_lower)
ci_upper_df <- as.data.frame(ci_upper)
p_values_df <- as.data.frame(p_values)

# Combine everything
results_df <- cbind(coefficients_df,
                    odds_ratios_df,
                    ci_lower_df,
                    ci_upper_df,
                    p_values_df)

# Set column names
colnames(results_df) <- c(paste0("Coef_", colnames(coefficients_df)),
                          paste0("OR_", colnames(odds_ratios_df)),
                          paste0("CIlower_", colnames(ci_lower_df)),
                          paste0("CIupper_", colnames(ci_upper_df)),
                          paste0("P_", colnames(p_values_df)))

results_df <- as_tibble(results_df) %>%
  add_column(group = c("Egalitarianists", "Utilitarianists"))

results_df <- results_df %>% 
  pivot_longer(all_of(names(results_df))[1:100]) %>% 
  separate(name, into = c("first", "rest"), sep = "_") %>%
  pivot_wider(id_cols = c("group", "rest"), names_from = first, values_from = value)

# Save to CSV
write.csv(
  results_df, "output/multinom_justice.csv",
  row.names = TRUE
)

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

# Plot proportions for each justice group
df_filtered <- df_long %>%
  filter(
    !(variable == "income" & (is.na(category) | category == "NA")),  # Exclude NA in income
    !(variable == "party" & (is.na(category) | category == "NA")),   # Exclude NA in party
    !(variable == "region" & category == "italian"),              # Exclude 'italian' from region
    !(variable == "gender" & category == "non-binary")            # Exclude 'non-binary' from gender
  )

ggplot(df_filtered, aes(x = justice_class, fill = category)) +
  geom_bar(position = "fill") +  # To normalize counts as a proportion
  facet_wrap(~ variable, scales = "free", ncol = 2) +  # Create facets for each variable
  labs(title = "Latent Profiles by Demographic Variables",
       x = "Latent Profile",
       y = "Proportion") +
  theme_minimal() +
  scale_fill_viridis_d(option = "D", drop = FALSE) +
  theme(legend.position = "bottom")
  