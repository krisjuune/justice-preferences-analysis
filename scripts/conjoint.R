library(tidyverse)
library(dplyr)
library(purrr) # for functional programming
library(forcats) # for better factorisation management
library(arrow)
library(broom)
library(cregg)
library(survey)
library(broom.helpers)
library(parameters)
library(gridExtra)
library(grid)

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

# loop over the two experiments and demographic variables, incl justice class
by_variables <- c("justice_class", 
                  "age", 
                  "gender", 
                  "region", 
                  "income", 
                  "education", 
                  "urbanness", 
                  "party", 
                  "trust")
                  # "satisfaction")

#TODO add citizen and renting (issue with their factorisation probs)

# Define the experiments
experiments <- c("heat", "pv")

# Loop over both experiments and the 'by' variables
for (experiment in experiments) {
  # choose the dataframe for each experiment
  df <- if (experiment == "heat") df_heat else df_pv
  
  # loop over each 'by' variable
  for (by in by_variables) {
    # save subgroup analyses for each variable
    result <- subgroup_mm(df, 
                          by = by, 
                          experiment = experiment, 
                          save_file = TRUE, 
                          get_plot = FALSE)
    
    # print message if completed
    print(paste("Completed:", experiment, "for", by))
  }
}

#TODO combine with above potentially
# initialize an empty list to store all the plots
all_plots <- list()
x_limits <- c(0.3, 0.7)
big_font <- 12
med_font <- 10
small_font <- 8

# for each experiment ('heat' and 'pv')
for (experiment in experiments) {
  # choose the dataframe for each experiment
  df <- if (experiment == "heat") df_heat else df_pv
  
  # initialize an empty list to store the plots for each 'by' variable
  experiment_plots <- list()
  
  # loop over each 'by' variable
  for (by_var in by_variables) {
    # filter out specific levels for 'gender' and 'region'
    if (by_var == "gender") {
      df_filtered <- df %>% 
        filter(gender %in% c("female", "male")) %>%
        droplevels()
    } else if (by_var == "region") {
      df_filtered <- df %>% 
        filter(region %in% c("german", "french")) %>%
        droplevels()
    } else {
      df_filtered <- df
    }
    
    # call the function to compute marginal means and plot
    result <- subgroup_mm(df = df_filtered,
                          by = by_var,
                          experiment = experiment,
                          save_file = FALSE,
                          get_plot = TRUE)
    
    # Add a title to each subplot specifying the 'by' variable
    plot_with_title <- result$plot +
      ggtitle(paste("By:", by_var)) +  
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = big_font),
        axis.title.x = element_text(size = med_font),
        axis.title.y = element_text(size = med_font),
        axis.text = element_text(size = small_font),
        legend.title = element_text(size = med_font),
        legend.text = element_text(size = small_font)
      ) +
      xlim(x_limits)
    
    # Store the plot in the experiment_plots list
    experiment_plots[[by_var]] <- plot_with_title
    
    # Save the marginal means (mm_results) to a file if desired
    # write.csv(result$mm_results, paste0(experiment, "_", by_var, "_MMs.csv"))
  }
  
  # Combine all the plots for this experiment into one large plot using gridExtra::grid.arrange
  combined_plot <- do.call(grid.arrange, c(experiment_plots, ncol = 3))  # Adjust 'ncol' to control the number of columns
  
  # Create a general title for the combined plot based on the experiment type
  general_title <- textGrob(paste("Marginal Means for", experiment, "Experiment"),
                            gp = gpar(fontsize = 16, fontface = "bold"))  # Adjust font size and style
  
  # Arrange the general title and the combined plots in a grid
  final_plot <- grid.arrange(general_title, combined_plot, ncol = 1, heights = c(0.1, 0.9))
  
  # Store the combined plot for the experiment
  all_plots[[experiment]] <- final_plot
  
  # Optionally, save the combined plot to a file
  ggsave(paste0("plot-files/MM-", experiment, "-combined.png"), 
         plot = combined_plot, 
         width = 25, 
         height = 20)
}

# check the combined plot
print(all_plots$heat)  # To display the combined plot for 'heat'
print(all_plots$pv)    # To display the combined plot for 'pv'

#TODO to be deleted
mm_party_pv <- cj(
  df_pv,
  Y ~ mix + imports + pv + tradeoffs + distribution,
  id = ~ID,
  estimate = "mm",
  by = ~party
)

#plot
plot(mm_party_pv, group = "party", vline = 0.5) +
  # theme_nice() +
  labs(title = "Choice outcome") +
  xlim(0.3, 0.7)

mm_trust_pv <- cj(
  df_pv,
  Y ~ mix + imports + pv + tradeoffs + distribution,
  id = ~ID,
  estimate = "mm",
  by = ~trust
)

#plot
plot(mm_trust_pv, group = "trust", vline = 0.5) +
  # theme_nice() +
  labs(title = "Choice outcome") +
  xlim(0.3, 0.7)

mm_satisfaction_pv <- cj(
  df_pv,
  Y ~ mix + imports + pv + tradeoffs + distribution,
  id = ~ID,
  estimate = "mm",
  by = ~satisfaction
)

#plot
plot(mm_satisfaction_pv, group = "satisfaction", vline = 0.5) +
  # theme_nice() +
  labs(title = "Choice outcome") +
  xlim(0.3, 0.7)

### heat
mm_party_heat <- cj(
  df_heat,
  Y ~ year + tax + ban + heatpump + energyclass + exemption,
  id = ~ID,
  estimate = "mm",
  by = ~party
)

#plot
plot(mm_party_heat, group = "party", vline = 0.5) +
  # theme_nice() +
  labs(title = "Choice outcome") +
  xlim(0.3, 0.7)

mm_trust_heat <- cj(
  df_heat,
  Y ~ year + tax + ban + heatpump + energyclass + exemption,
  id = ~ID,
  estimate = "mm",
  by = ~trust
)

#plot
plot(mm_trust_heat, group = "trust", vline = 0.5) +
  # theme_nice() +
  labs(title = "Choice outcome") +
  xlim(0.3, 0.7)

mm_satisfaction_heat <- cj(
  df_heat,
  Y ~ year + tax + ban + heatpump + energyclass + exemption,
  id = ~ID,
  estimate = "mm",
  by = ~satisfaction
)

#plot
plot(mm_satisfaction_heat, group = "satisfaction", vline = 0.5) +
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
  df_heat %>% select(ID, 
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
  df_pv %>% select(ID, 
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
  distinct(ID, .keep_all = TRUE) 

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
df_just$justice_class <- relevel(df_just$justice_class, ref = "universal")
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

# Convert coefficients, odds ratios, and p-values to data frames
coefficients_df <- as.data.frame(coefficients)
odds_ratios_df <- as.data.frame(odds_ratios)
p_values_df <- as.data.frame(p_values)

# Combine coefficients, odds ratios, and p-values into one data frame
results_df <- cbind(coefficients_df, odds_ratios_df, p_values_df)

# Update column names
colnames(results_df) <- c(paste0("Coefficient_", colnames(coefficients_df)),
                          paste0("OddsRatio_", colnames(odds_ratios_df)),
                          paste0("P_value_", colnames(p_values_df)))

# Save the results to a CSV file
write.csv(results_df, "data/multinom_justice.csv", 
          row.names = TRUE)

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
  # scale_fill_manual(values = c("red", "blue", "green", "orange", "purple", "yellow")) +  # Define specific colors
  theme(legend.position = "bottom")
  