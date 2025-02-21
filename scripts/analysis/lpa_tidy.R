library(mclust)
library(tidyLPA)
library(nnet)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(patchwork)
library(grid)
library(Matrix)
library(here)
source("functions/r-assist.R")

lpa_raw <- read.csv(here("data", "lpa_input.csv"))
lpa_raw <- lpa_raw |>
  mutate(
    speeder = as.logical(speeder),
    laggard = as.logical(laggard),
    inattentive = as.logical(inattentive)
  )
lpa_data <- lpa_raw |>
  filter_respondents() |>
  drop_na()
# 5 missing for respondents that dropped out
# during or before the justice section

########################## LPA models ############################
set.seed(42)

# run model and store results
lpa_columns <- lpa_data |>
  select(egalitarian, limitarian, sufficientarian, utilitarian)
lpa_columns <- lpa_columns |>
  mutate_if(is.factor, as.numeric) |>
  mutate_if(is.character, as.numeric)

#SLOW perform lpa - this takes a minute or two!
lpa_results <- estimate_profiles(
  lpa_columns,
  1:8,
  # choosing the mclust model EEI for class-invariant parameterization
  variances = "equal",
  covariances = "zero"
)

# collect the fit statistics for models with 1 to 8 profiles
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

# reshape the data for plotting
fit_stats_plot <- fit_stats |>
  select(G, AIC, AWE, BIC, SABIC, ICL) |>
  mutate(ICL = ICL * -1)

fit_stats_long <- pivot_longer(fit_stats_plot,
                               cols = -G,
                               names_to = "statistic",
                               values_to = "value") |>
  mutate(
    statistic = factor(
      statistic,
      levels = c(
        "ICL",
        "AWE",
        "BIC",
        "SABIC",
        "AIC"
      )
    )
  )

# create elbow plot of fit statistics
lpa_elbow_plot <- ggplot(fit_stats_long,
                         aes(x = G,
                             y = value,
                             color = statistic,
                             shape = statistic,
                             fill = statistic)) +
  geom_line(linewidth = .8, alpha = .7) +
  geom_point(size = 2, stroke = 1.2) +
  labs(title = "Elbow Plot of Fit Statistics for LPA",
       x = "Number of Profiles (G)",
       y = "Fit Statistics") +
  theme_classic() +
  theme(text = element_text(size = 12)) +
  scale_color_viridis_d(end = .85) +
  scale_shape_manual(values = 21:25) +
  scale_fill_viridis_d(end = .85)

# save fit stats stuff
write.csv(fit_stats, here("data", "lpa_fit_stats.csv"), row.names = TRUE)

ggsave(
  here("output", "lpa_stats_elbow_plot.png"), 
  plot = lpa_elbow_plot,
  width = 9, height = 5
)

# save data with IDs and justice class
class_assignments_g3 <- lpa_results[[3]]$dff$Class
lpa_data_g3 <- lpa_data |>
  mutate(justice_class = class_assignments_g3)
lpa_class_g3 <- lpa_raw |>
  left_join(lpa_data_g3 |>
              select(ID, justice_class),
            by = "ID")

write.csv(
  lpa_class_g3,
  here("data", "lpa_data.csv"),
  row.names = TRUE
)

class_assignments_g4 <- lpa_results[[4]]$dff$Class
lpa_data_g4 <- lpa_data |>
  mutate(justice_class = class_assignments_g4)
lpa_class_g4 <- lpa_raw |>
  left_join(lpa_data_g4 |>
              select(ID, justice_class),
            by = "ID")

write.csv(
  lpa_class_g4,
  here("data", "lpa_data_g4.csv"),
  row.names = TRUE
)
