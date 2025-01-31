library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(patchwork)
library(here)

df_push <- read_csv(here("data", "mm_stringency.csv")) |>
  filter(outcome == "choice") |>
  mutate(
    level = factor(
      level,
      levels = c(
        "High stringency",
        "Medium stringency",
        "Low stringency"
      ),
      labels = c(
        "High stringency",
        "Medium stringency",
        "Low stringency"
      )
    ),
    experiment = factor(
      experiment,
      levels = c(
        "pv",
        "heat"
      ),
      labels = c(
        "Renewable energy scale-up",
        "Heating sector decarbonisation"
      )
    ),
    justice_class = factor(
      justice_class,
      levels = c(
        "egalitarian", "universal", "utilitarian"
      ),
      labels = c(
        "Egalitarian", "Universal", "Utilitarian"
      )
    )
  )

df_util <- read_csv(here("data", "mm_util_packages.csv")) |>
  filter(outcome == "choice") |>
  mutate(
    level = factor(
      level,
      levels = c(
        "Utilitarian packages",
        "Egalitarian packages",
        "Other packages"
      ),
      labels = c(
        "Utilitarian packages",
        "Egalitarian packages",
        "Other packages"
      )
    ),
    experiment = factor(
      experiment,
      levels = c(
        "pv",
        "heat"
      ),
      labels = c(
        "Renewable energy scale-up",
        "Heating sector decarbonisation"
      )
    ),
    justice_class = factor(
      justice_class,
      levels = c(
        "egalitarian", "universal", "utilitarian"
      ),
      labels = c(
        "Egalitarian", "Universal", "Utilitarian"
      )
    )
  )


plot_policy_features <- function(data){
  ggplot(data,
         aes(
           x = estimate,
           y = level,
           colour = justice_class
         )) +
    geom_point(
      size = 3,
      position = position_dodge(width = 0.5)
    ) +
    geom_errorbarh(
      aes(
      xmax = estimate + std.error,
      xmin = estimate - std.error
      ),
      height = .2,
      position = position_dodge(width = 0.5)
    ) +
    facet_wrap(
      vars(experiment)
    ) +
    geom_vline(
      xintercept = .5,
      linetype = 2,
      colour = "gray40",
      size = .3
    ) +
    scale_color_viridis_d(end = .8) +
    labs(
        colour = NULL,
        shape = NULL,
        y = NULL,
        x = "Marginal means"
      )
}

push_plot <- df_push |>
  plot_policy_features()

util_plot <- df_util |>
  plot_policy_features()

mm_policy_packages_plot <- (push_plot / util_plot) +
  plot_layout(guides = "collect", axis_titles = "collect") +
  plot_annotation(tag_levels = "A") &
  scale_x_continuous(limits = c(.32, .68)) &
  theme_classic() &
  theme(
    legend.position = "right",
    text = element_text(size = 11),
    strip.background = element_rect(size = 0),
    strip.text.x = element_text(size = 11, face = "bold")
  )

ggsave(
  plot = mm_policy_packages_plot,
  here("output", "mm_policy_packages_plot.png"),
  height = 6, width = 9
)

mm_policy_packages_plot
