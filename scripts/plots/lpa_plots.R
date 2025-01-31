library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(patchwork)
library(here)


lpa_data <- read_csv(here("data", "lpa_data.csv"))[, -1] |>
  filter(!is.na(justice_class)) |>
  mutate(
    justice_class = factor(
        justice_class,
        levels = c(
            "2", "1", "3"
        ), 
        labels = c(
            "Egalitarian", "Universal", "Utilitarian"
        )
    )
  )

# calculate scores for each principle
mean_values <- lpa_data %>%
  group_by(justice_class) %>%
  summarize(
    utilitarian = mean(utilitarian, na.rm = TRUE),
    egalitarian = mean(egalitarian, na.rm = TRUE),
    sufficientarian = mean(sufficientarian, na.rm = TRUE),
    limitarian = mean(limitarian, na.rm = TRUE)
  )

mean_values_long <- mean_values %>%
  pivot_longer(cols = utilitarian:limitarian,
               names_to = "principle",
               values_to = "value")

mean_values_long <- mean_values_long %>%
  mutate(
    ymin = value - 0.95,
    ymax = value + 0.95,
    principle = factor(
        principle,
        levels = c(
            "egalitarian", "limitarian", "sufficientarian", "utilitarian"
        ), 
        labels = c(
            "Equal outcomes", "Limitarian", "Sufficientarian", "Utilitarian"
        )
    )
  )

# Plot principles on x-axis and latent profiles as colors in the legend
plot_profile_principles <- ggplot(mean_values_long,
                                  aes(x = principle,
                                      y = value,
                                      color = factor(justice_class),
                                      group = factor(justice_class),
                                      shape = factor(justice_class))) +
  geom_line(size = .5, alpha = .3, position = position_dodge(width = 0.2)) +
  geom_point(size = 3, position = position_dodge(width = 0.2)) +
  geom_errorbar(
    aes(
        ymin = ymin,
        ymax = ymax),
        width = 0.2,
        size = 0.6,
  position = position_dodge(width = 0.2)) +
  labs(title = "Mean scores for justice principles",
       color = "Latent profile", shape = "Latent profile") +
  theme_classic() +
  scale_color_viridis_d(end = .8)

# get nr of people in each justice group
plot_profile_counts <- ggplot(lpa_data,
                              aes(x = justice_class, fill = justice_class)) +
  geom_bar(aes(y = after_stat(count / sum(count))), alpha = .8, width = .65) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Relative profile sizes") +
  theme_classic() +
  theme(
    legend.position = "none"
  ) +
  scale_fill_viridis_d(end = .8)

plot_profile_counts
plot_profile_principles

lpa_results <- plot_profile_counts +
  plot_profile_principles +
  plot_layout(ncol = 2, widths = c(1, 2)) &
  theme(
    text = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

lpa_results

ggsave(
  here("output", "lpa_results.png"), 
  plot = lpa_results, 
  height = 5, width = 11
)


# Plotting the profiles per participant
# Pivot the lpa_data into long format for plotting each participant
participant_long <- lpa_data %>%
  pivot_longer(cols = justice_general_1:justice_subsidy_4, 
               names_to = "variable", 
               values_to = "value")

plot_participants <- ggplot(participant_long, aes(x = variable, y = value, group = ID, color = factor(justice_class))) +
  geom_line(alpha = 0.1, size = 0.5) +
  labs(title = "Individual Participant Profiles",
       x = "Principle",
       y = "Score",
       color = "Latent Profile") +
  theme_minimal() +
  guides(color = guide_legend(override.aes = list(size = 2)))

# Display the updated plot with thicker legend lines
plot_participants