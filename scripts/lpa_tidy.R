library(mclust)
library(tidyLPA)
library(nnet)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(patchwork)
library(grid)
library(Matrix)
source("functions/r-assist.R")

lpa_raw <- read.csv("data/lpa_input.csv")
lpa_raw <- lpa_raw %>%
  mutate(
    speeder = as.logical(speeder),
    laggard = as.logical(laggard),
    inattentive = as.logical(inattentive)
  )
lpa_data <- lpa_raw %>%
  filter_respondents() %>%
  drop_na()
# 5 missing for respondents that dropped out
# during or before the justice section

########################## LPA models ############################

# run model and store results 
lpa_columns <- lpa_data %>%
  select(-ID, -speeder, -laggard, -inattentive)
lpa_columns <- lpa_columns %>%
  mutate_if(is.factor, as.numeric) %>%
  mutate_if(is.character, as.numeric)

# Perform LPA on the subset data
lpa_results <- estimate_profiles(
  lpa_columns, 
  1:8, 
  # choosing the mclust model EEI for class-invariant parameterization
  variances = "equal", 
  covariances = "zero"
)

# Collect the fit statistics for models with 1 to 8 profiles
fit_stats <- data.frame(
  G = 1:8,
  AIC = sapply(1:8, function(x) lpa_results[[x]]$fit[[4]]),
  AWE = sapply(1:8, function(x) lpa_results[[x]]$fit[[5]]),
  BIC = sapply(1:8, function(x) lpa_results[[x]]$fit[[6]]),
  SABIC = sapply(1:8, function(x) lpa_results[[x]]$fit[[10]]),
  ICL = sapply(1:8, function(x) lpa_results[[x]]$fit[[11]]),
  entropy = sapply(1:8, function(x) lpa_results[[x]]$fit[[12]]),
  prob_min = sapply(1:8, function(x) lpa_results[[x]]$fit[[13]]),
  prob_max = sapply(1:8, function(x) lpa_results[[x]]$fit[[14]])
)

# Calculate proportions for each G
min_proportions <- numeric(length(lpa_results))

# Get smallest proportions for each model (each G)
for (i in seq_along(lpa_results)) {
  class_assignments <- lpa_results[[i]]$dff$Class
  class_proportions <- table(class_assignments) / length(class_assignments) * 100
  min_proportions[i] <- round(min(class_proportions), 1)
}

fit_stats$min_proportion <- min_proportions

# write fit stats to file
write.csv(fit_stats, "data/lpa_fit_stats.csv", row.names = TRUE)


# reshape the data for plotting
fit_stats_plot <- fit_stats %>%
  select(AIC, AWE, BIC, SABIC, ICL) %>%
  mutate(ICL = ICL * -1)
fit_stats_long <- pivot_longer(fit_stats_plot, 
                               cols = -G, 
                               names_to = "Statistic", 
                               values_to = "Value")

# create elbow plot of fit statistics
ggplot(fit_stats_long, aes(x = G, y = Value, color = Statistic)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Elbow Plot of Fit Statistics for LPA",
       x = "Number of Profiles (G)",
       y = "Fit Statistics") +
  theme_minimal() +
  theme(text = element_text(size = 12))

######################### save LPA data ##########################

# save data with IDs and justice class
class_assignments <- lpa_results[[3]]$dff$Class
lpa_data <- lpa_data %>%
  mutate(justice_class = class_assignments)
lpa_class <- lpa_raw %>%
  left_join(lpa_data %>% 
              select(ID, justice_class),
            by = "ID")

# write.csv(lpa_class, "data/lpa_data.csv", row.names = TRUE)

########################## run LPA ###############################
# LPA with mclust
# lpa_columns <- lpa_data[, c("utilitarian",
#                             "egalitarian",
#                             "sufficientarian",
#                             "limitarian")]

lpa_columns <- lpa_data[, c('justice_general_1', 
                            'justice_tax_1', 
                            'justice_subsidy_1',
                            'justice_general_2', 
                            'justice_tax_2',
                            'justice_subsidy_2',
                            'justice_general_3',
                            'justice_tax_3',
                            'justice_subsidy_3',
                            'justice_general_4',
                            'justice_tax_4',
                            'justice_subsidy_4')]

lpa_model_mclust <- Mclust(lpa_columns)

summary(lpa_model_mclust) # check output
print(lpa_model_mclust) # best model according to BIC

# classification of each observation
lpa_columns$mclust_class <- lpa_model_mclust$classification

#TODO write code for choosing the nr of profiles automatically
# and rerunning the model with just that value
# tidy the mclust LPA model with tidyLPA
lpa_model_tidy <- lpa_columns %>%
  select(-mclust_class) %>%  # exclude the mclust class column if included
  single_imputation() %>%    # handle missing data if necessary
  estimate_profiles(n_profiles = 1:8,
                    package = "mclust")

# summarize the tidyLPA model
lpa_model_tidy
# 3 classes has the highest entropy,
# BIC is very similar and lowest for 5 classes

# compare models
compare_solutions(lpa_model_tidy)
summary(lpa_model_tidy)

# visualize the profiles from tidyLPA
plot_profiles(lpa_model_tidy)

# add the profile assignments to your original dataframe
model_data <- get_data(lpa_model_tidy) # get the data with class assignments
profile_assignments <- model_data$Class # extract the class assignments
lpa_data <- lpa_data %>%
  mutate(justice_class = profile_assignments)
lpa_class <- lpa_raw %>%
  left_join(lpa_data %>% select(ID, justice_class), by = "ID")

counts <- table(lpa_data$justice_class)
# class 3: egalitarian (and sufficientarian and limitarian) -- 1032 (bi 1029)
# class 2: universal (scores for all around 2) -- 788 (bi 584)
# class 1: utilitarian (v low scores for all, highest for util) -- 196 (bi 406)

# write.csv(lpa_class, "data/lpa_data.csv", row.names = TRUE)

########################## test plots ################################
plot1 <- ggplot(lpa_data,
                aes(x = factor(justice_class),
                    fill = factor(justice_class))) +
  geom_bar() +
  labs(title = "Distribution of Classes by tidyLPA",
       x = "Latent Profile",
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

# Step 1: Compute the mean values for each class
# mean_values <- lpa_data %>%
#   group_by(justice_class) %>%
#   summarize(
#     utilitarian = mean(utilitarian, na.rm = TRUE),
#     egalitarian = mean(egalitarian, na.rm = TRUE),
#     sufficientarian = mean(sufficientarian, na.rm = TRUE),
#     limitarian = mean(limitarian, na.rm = TRUE)
#   )


# mean_values <- lpa_data %>%
#   group_by(justice_class) %>%
#   summarize(
#     utilitarian = mean(utilitarian, na.rm = TRUE),
#     egalitarian = mean(egalitarian, na.rm = TRUE),
#     sufficientarian = mean(sufficientarian, na.rm = TRUE),
#     limitarian = mean(limitarian, na.rm = TRUE)
#   )

mean_values <- lpa_data %>%
    group_by(justice_class) %>%
    summarize(
      utilitarian_gen = mean(justice_general_1, na.rm = TRUE),
      utilitarian_tax = mean(justice_tax_1, na.rm = TRUE),
      utilitarian_sub = mean(justice_subsidy_1, na.rm = TRUE),
      egalitarian_gen = mean(justice_general_2, na.rm = TRUE),
      egalitarian_tax = mean(justice_tax_2, na.rm = TRUE),
      egalitarian_sub = mean(justice_subsidy_2, na.rm = TRUE),
      sufficientarian_gen = mean(justice_general_3, na.rm = TRUE),
      sufficientarian_tax = mean(justice_tax_3, na.rm = TRUE),
      sufficientarian_sub = mean(justice_subsidy_3, na.rm = TRUE),
      limitarian_gen = mean(justice_general_4, na.rm = TRUE),
      limitarian_tax = mean(justice_tax_4, na.rm = TRUE),
      limitarian_sub = mean(justice_subsidy_4, na.rm = TRUE),
    )


# Step 2: Pivot the data into long format using pivot_longer
mean_values_long <- mean_values %>%
  pivot_longer(cols = utilitarian_gen:limitarian_sub, 
               names_to = "variable", 
               values_to = "value")

mean_values_long <- mean_values_long %>%
  mutate(
    ymin = value - 0.95,  
    ymax = value + 0.95
  )

# # Step 3: Plot data per latent profile
# ggplot(mean_values_long, aes(x = factor(justice_class), y = value, color = variable, group = variable)) +
#   geom_line(size = 1.5) +
#   geom_point(size = 3) +
#   labs(title = "Profiles Across Latent Classes",
#        x = "Latent Profile",
#        y = "Mean Score",
#        color = "Variable") +
#   theme_minimal()

# Plotting with variables on x-axis and latent profiles as colors in the legend
plot2 <- ggplot(mean_values_long, aes(x = variable, y = value, color = factor(justice_class), group = factor(justice_class))) +
  geom_line(size = 1.5) +      # Draws lines connecting points within each latent profile
  geom_point(size = 3) +       # Adds points at each mean value
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2, size = 0.8) +  # Adds error bars
  labs(title = "Profiles Across Latent Classes",
       x = "Variable",
       y = "Mean Score",
       color = "Latent Profile") +
  theme_minimal()

plot2
plot1

plot1 + plot2 + plot_layout(ncol = 2)

# Plotting the profiles per participant
# Pivot the lpa_data into long format for plotting each participant
participant_long <- lpa_data %>%
  pivot_longer(cols = justice_general_1:justice_subsidy_4, 
               names_to = "variable", 
               values_to = "value")

plot_participants <- ggplot(participant_long, aes(x = variable, y = value, group = ID, color = factor(justice_class))) +
  geom_line(alpha = 0.1, size = 0.5) +   # Thin and transparent lines for individual participants
  labs(title = "Individual Participant Profiles",
       x = "Principle",
       y = "Score",
       color = "Latent Profile") +
  theme_minimal() +
  guides(color = guide_legend(override.aes = list(size = 2)))

# Display the updated plot with thicker legend lines
plot_participants


