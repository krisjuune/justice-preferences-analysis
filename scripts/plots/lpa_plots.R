library(ggplot2)
library(ggdist)
library(gghalves)
library(dplyr)
library(tidyr)
library(readr)
library(patchwork)
library(here)
library(scales)

main_text_size <- 14
set.seed(42)

lpa_data <- read_csv(
  here("data", "lpa_data.csv"),
  show_col_types = FALSE
)[, -1] |>
  filter(!is.na(justice_class)) |>
  mutate(
    justice_class = factor(
      justice_class,
      levels = c(
        "1", "3", "2"
      ),
      labels = c(
        "Egalitarianists", "Universalists", "Utilitarianists"
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
      Support = ~ mean(. > 7) * 100
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


pivot_participant_profiles_long <- function(
  data_wide,
  shorten_labels = FALSE,
  get_z = FALSE
) {
  labels_long <- c(
    "Utilitarian",
    "Sufficientarian",
    "Limitarian",
    "Equal outcomes"
  )

  labels_short <- c(
    "Util",
    "Suff",
    "Lim",
    "Equal"
  )

  data_long <- data_wide |>
    pivot_longer(
      cols = c("utilitarian", "limitarian", "sufficientarian", "egalitarian"),
      names_to = "principle",
      values_to = "value"
    )

  if (get_z) {
    data_long <- data_long |>
      group_by(principle) |>
      # get z scores
      mutate(value = scale(value)[, 1]) |>
      ungroup()
  }

  data_long <- data_long |>
    mutate(
      principle = factor(
        principle,
        levels = c("utilitarian", "limitarian", "sufficientarian", "egalitarian"),
        labels = if (shorten_labels) labels_short else labels_long
      )
    )

  return(data_long)
}

## get profiles per participant
plot_participant_profiles <- function(data) {
  plot <- ggplot(data, aes(
    x = principle,
    y = value,
    group = id,
    color = justice_class
  )) +
    geom_line(alpha = 0.4, size = 0.5) +
    labs(
      title = NULL,
      x = "Justice principle",
      y = "z-score",
      color = NULL
    ) +
    theme_classic() +
    scale_color_viridis_d(
      end = .8,
      guide = guide_legend(override.aes = list(alpha = 1))
    ) +
    theme(
      legend.position = "none",
      text = element_text(size = main_text_size),
      strip.background = element_rect(size = 0),
      strip.text.x = element_text(size = main_text_size, face = "bold")
    )

  plot
}

# create raincloud plot
plot_raincloud <- lpa_data |>
  pivot_participant_profiles_long() |>
  ggplot(aes(
    x = principle,
    y = value,
    fill = justice_class,
    colour = justice_class
  )) +
  stat_halfeye(
    alpha = .6,
    adjust = 2.2,
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
  scale_y_continuous(breaks = seq(0, 15, 1)) +
  scale_fill_viridis_d(end = .8) +
  scale_colour_viridis_d(end = .8) +
  facet_wrap(~justice_class, ncol = 1) +
  labs(
    y = "Sum score",
    x = "Justice principle",
    colour = NULL,
    fill = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    text = element_text(size = main_text_size),
    strip.background = element_rect(size = 0),
    strip.text.x = element_text(size = main_text_size, face = "bold")
  )



# get plots
plot_lpa_results <- plot_lpa_results(lpa_data)

plot_participants <- lpa_data |>
  group_by(justice_class) |>
  slice_sample(n = 50) |>
  pivot_participant_profiles_long(
    shorten_labels = TRUE,
    get_z = TRUE
  ) |>
  plot_participant_profiles() +
  facet_wrap(~justice_class, ncol = 3)

# make compound raincloud plot
panel_a <- plot_raincloud

justice_pie_data <- data.frame(
  justice_class = names(justice_class_proportions),
  proportion = as.numeric(justice_class_proportions)
) |>
  mutate(label = paste0(
    justice_class,
    "\n",
    scales::percent(proportion, accuracy = 0.1)
  ))

panel_b_pie <- ggplot(
  justice_pie_data,
  aes(x = "", y = proportion, fill = justice_class)
) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_viridis_d(end = .8, alpha = .75) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(1, 1, 1, 1, "pt")
  )

combined_plot_pie <- panel_a +
  inset_element(panel_b_pie, left = 0.02, bottom = 0.8, right = 0.255, top = 1.1) +
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      plot.tag = element_text(size = 16)
    )
  )

justice_bar_data <- data.frame(
  justice_class = c("Egalitarianists", "Universalists", "Utilitarianists"),
  count = as.numeric(table(lpa_data$justice_class))
) |>
  dplyr::mutate(
    proportion = count / sum(count),
    percent_label = scales::percent(proportion, accuracy = 0.1)
  )

panel_b <- ggplot(justice_bar_data, aes(x = justice_class, y = count, fill = justice_class)) +
  geom_col(width = 0.6, color = "white") +
  geom_text(
    aes(label = percent_label),
    vjust = -0.4,
    size = 4.5,
    fontface = "bold"
  ) +
  scale_fill_viridis_d(end = .8, alpha = 0.75) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_classic() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 5, "pt"),
    text = element_text(size = main_text_size),
  ) +
  ylab("Number of respondents")

combined_plot <- panel_a + panel_b +
  plot_layout(
    ncol = 2,
    widths = c(2.5, 1)
  )

# save stuff
ggsave(
  here("output", "lpa_raincloud_compound.png"),
  plot = combined_plot,
  height = 8, width = 10
)

ggsave(
  here("output", "lpa_results.png"),
  plot = plot_lpa_results,
  height = 5, width = 11
)

ggsave(
  here("output", "lpa_participant_profiles_z.png"),
  plot = plot_participants,
  height = 6, width = 11
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
