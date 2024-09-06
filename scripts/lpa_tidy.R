library(mclust)
library(tidyLPA)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(patchwork)

lpa_raw <- read.csv("data/lpa_input.csv")
lpa_raw <- lpa_raw %>%
  mutate(
    speeder = as.logical(speeder),
    laggard = as.logical(laggard),
    inattentive = as.logical(inattentive)
  )
lpa_data <- lpa_raw %>%
  filter(!speeder & !laggard & !inattentive) %>%
  drop_na() # 5 missing for respondents that dropped out during or before the justice section

# LPA with mclust
lpa_columns <- lpa_data[, c('utilitarian', 'egalitarian', 'sufficientarian', 'limitarian')]
lpa_model_mclust <- Mclust(lpa_columns)

summary(lpa_model_mclust) # check output
print(lpa_model_mclust) # best model according to BIC

lpa_columns$mclust_class <- lpa_model_mclust$classification # classification of each observation

#TODO write code for choosing the nr of profiles automatically and rerunning the model with just that value
# LPA with tidyLPA
lpa_model_tidy <- lpa_columns %>%
  select(-mclust_class) %>%  # exclude the mclust class column if included
  single_imputation() %>%    # handle missing data if necessary
  estimate_profiles(n_profiles = 3:3, # adjust the range for the number of profiles you want to test
                    package = "mclust") # using mclust as the estimation package

# summarize the tidyLPA model
lpa_model_tidy # 3 classes has the highest entropy, BIC is very similar and lowest for 5 classes

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
# class 1: utilitarian (and minimally sufficientarian and limitarian) -- 196 (bi 406)

write.csv(lpa_class, "data/lpa_data.csv", row.names = TRUE)

########################## test plots ################################
plot1 <- ggplot(lpa_data, aes(x = factor(justice_class), fill = factor(justice_class))) +
  geom_bar() +
  labs(title = "Distribution of Classes by tidyLPA",
       x = "Latent Profile",
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

# Step 1: Compute the mean values for each class
mean_values <- lpa_data %>%
  group_by(justice_class) %>%
  summarize(
    utilitarian = mean(utilitarian, na.rm = TRUE),
    egalitarian = mean(egalitarian, na.rm = TRUE),
    sufficientarian = mean(sufficientarian, na.rm = TRUE),
    limitarian = mean(limitarian, na.rm = TRUE)
  )

# Step 2: Pivot the data into long format using pivot_longer
mean_values_long <- mean_values %>%
  pivot_longer(cols = utilitarian:limitarian, 
               names_to = "variable", 
               values_to = "value")

mean_values_long <- mean_values_long %>%
  mutate(
    ymin = value - 0.95,  
    ymax = value + 0.95
  )

# Step 3: Plot data per latent profile
ggplot(mean_values_long, aes(x = factor(justice_class), y = value, color = variable, group = variable)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  labs(title = "Profiles Across Latent Classes",
       x = "Latent Profile",
       y = "Mean Score",
       color = "Variable") +
  theme_minimal()

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

plot1 + plot2 + plot_layout(ncol = 2)


############################ demographics ################################
