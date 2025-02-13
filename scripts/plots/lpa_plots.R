library(ggplot2)
library(ggdist)
library(gghalves)
library(dplyr)
library(tidyr)
library(readr)
library(patchwork)
library(here)
library(scales)

lpa_data <- read_csv(
  here("data", "lpa_data.csv"),
  show_col_types = FALSE
)[, -1] |>
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

# get proportion table and median scores
justice_class_proportions <- as.list(prop.table(table(lpa_data$justice_class)))
principles_summary <- lpa_data |>
  summarise(across(c(utilitarian, egalitarian, sufficientarian, limitarian), list(
      Median = median,
      Mean = mean,
      SD = sd,
      Support = ~ mean(. > 0) * 100
    ), .names = "{.col}_{.fn}")) |>
  pivot_longer(
    cols = everything(),
    names_to = c("Column", "Statistic"),
    names_sep = "_"
  ) |>
  pivot_wider(
    names_from = "Statistic",
    values_from = "value"
  )

plot_lpa_results <- function(data) {
  mean_values <- data |>
    group_by(justice_class) |>
    summarize(
      utilitarian_mean = mean(utilitarian, na.rm = TRUE),
      utilitarian_se = sd(utilitarian, na.rm = TRUE) / sqrt(n()),
      egalitarian_mean = mean(egalitarian, na.rm = TRUE),
      egalitarian_se = sd(egalitarian, na.rm = TRUE) / sqrt(n()),
      sufficientarian_mean = mean(sufficientarian, na.rm = TRUE),
      sufficientarian_se = sd(sufficientarian, na.rm = TRUE) / sqrt(n()),
      limitarian_mean = mean(limitarian, na.rm = TRUE),
      limitarian_se = sd(limitarian, na.rm = TRUE) / sqrt(n())
    ) |>
    pivot_longer(
      cols = starts_with("utilitarian_mean"):starts_with("limitarian_se"),
      names_to = c("principle", ".value"),
      names_pattern = "(.*)_(mean|se)"
    ) |>
    mutate(
      lower = mean - qt(0.975, n() - 1) * se,
      upper = mean + qt(0.975, n() - 1) * se,
      principle = factor(
        principle,
        levels = c("egalitarian", "limitarian", "sufficientarian", "utilitarian"),
        labels = c("Equal outcomes", "Limitarian", "Sufficientarian", "Utilitarian")
      )
    )

  plot_profile_principles <- ggplot(
    mean_values,
    aes(
      x = principle,
      y = mean,
      color = factor(justice_class),
      group = factor(justice_class),
      shape = factor(justice_class)
    )
  ) +
    geom_line(linewidth = .5, alpha = .3, position = position_dodge(width = 0.2)) +
    geom_point(size = 3, position = position_dodge(width = 0.2)) +
    geom_errorbar(
      aes(ymin = lower, ymax = upper),
      width = 0.2,
      size = 0.6,
      position = position_dodge(width = 0.2)
    ) +
    labs(
      title = "B. Mean scores for justice principles",
      color = NULL,
      shape = NULL
    ) +
    theme_classic() +
    scale_color_viridis_d(end = .8)

  plot_profile_counts <- ggplot(
    data,
    aes(x = justice_class, fill = justice_class)
  ) +
    geom_bar(aes(y = after_stat(count / sum(count))), alpha = .8, width = .65) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "A. Relative profile sizes") +
    theme_classic() +
    theme(legend.position = "none") +
    scale_fill_viridis_d(end = .8)

  lpa_results <- plot_profile_counts +
    plot_profile_principles +
    plot_layout(
      ncol = 2,
      widths = c(1, 2.5)
    ) &
    theme(
      text = element_text(size = 14),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )

  return(lpa_results)
}


pivot_participant_profiles_long <- function(data_wide) {
  data_wide |>
    pivot_longer(
      cols = c(
        "egalitarian",
        "sufficientarian",
        "limitarian",
        "utilitarian"
      ),
      names_to = "principle",
      values_to = "value"
    ) |>
    mutate(
      principle = factor(
        principle,
        levels = c(
          "egalitarian",
          "limitarian",
          "sufficientarian",
          "utilitarian"
        ),
        labels = c(
          "Equal outcomes",
          "Limitarian",
          "Sufficientarian",
          "Utilitarian"
        )
      )
    )
}

## get profiles per participant
plot_participant_profiles <- function(data) {
  plot <- ggplot(data, aes(
    x = principle,
    y = value,
    group = ID,
    color = justice_class
  )) +
    geom_line(alpha = 0.3, size = 0.5) +
    labs(
      title = "Individual Participant Profiles",
      x = "Principle",
      y = "Score",
      color = "Latent Profile"
    ) +
    theme_classic() +
    scale_color_viridis_d(
      end = .8,
      guide = guide_legend(override.aes = list(alpha = 1))
    )

  plot
}

# create raincloud plot
plot_raincloud <- lpa_data |>
  pivot_participant_profiles_long() |>
  mutate(
    justice_class_prop = case_when(
      justice_class == "Egalitarian" ~ paste0("Egalitarian (", percent(justice_class_proportions$Egalitarian, accuracy = 0.1), ")"),
      justice_class == "Universal" ~ paste0("Universal (", percent(justice_class_proportions$Universal, accuracy = 0.1), ")"),
      justice_class == "Utilitarian" ~ paste0("Utilitarian (", percent(justice_class_proportions$Utilitarian, accuracy = 0.1), ")"),
    )
  ) |>
  ggplot(aes(
    x = principle,
    y = value,
    fill = justice_class_prop,
    colour = justice_class_prop,
  )) +
  stat_halfeye(
    alpha = .6,
    adjust = 1.5,
    width = .6,
    .width = c(.5, .95),
    density = "bounded"
  ) +
  geom_boxplot(
    width = .15,
    fill = "white",
    outlier.shape = NA
  ) +
  geom_half_point(
    side = "l",
    range_scale = .4,
    alpha = .2
  ) +
  coord_flip() +
  scale_y_continuous(breaks = seq(-10, 10, 1)) +
  scale_fill_viridis_d(end = .8) +
  scale_colour_viridis_d(end = .8) +
  facet_wrap(~justice_class_prop, ncol = 1) +
  labs(
    y = "Sum score",
    x = "Justice principle",
    colour = NULL,
    fill = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    text = element_text(size = 14),
    strip.background = element_rect(size = 0),
    strip.text.x = element_text(size = 14, face = "bold")
  )

# get plots
plot_lpa_results <- plot_lpa_results(lpa_data)

plot_participants <- lpa_data |>
  pivot_participant_profiles_long() |>
  plot_participant_profiles()

# save stuff
ggsave(
  here("output", "lpa_results.png"),
  plot = plot_lpa_results,
  height = 5, width = 11
)

ggsave(
  here("output", "lpa_participant_profiles.png"),
  plot = plot_participants,
  height = 7, width = 9
)

ggsave(
  here("output", "lpa_raincloud.png"),
  plot = plot_raincloud,
  height = 10, width = 9
)

write_csv(
  as_tibble(justice_class_proportions),
  here("data", "lpa_proportions.csv")
)

write_csv(
  principles_summary,
  here("data", "lpa_principle_support.csv")
)
