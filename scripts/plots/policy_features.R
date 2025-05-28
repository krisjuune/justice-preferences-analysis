library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(patchwork)
library(here)

df_push <- read_csv(
  here("data", "mm_stringency.csv"),
  show_col_types = FALSE
) |>
  filter(outcome == "choice") |>
  mutate(
    level = factor(
      level,
      levels = c(
        "Low stringency",
        "Medium stringency",
        "High stringency"
      ),
      labels = c(
        "Low stringency",
        "Medium stringency",
        "High stringency"
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
        "Egalitarianists", "Universalists", "Utilitarianists"
      ),
      labels = c(
        "Egalitarianists", "Universalists", "Utilitarianists"
      )
    )
  )

df_util <- read_csv(
  here("data", "mm_instrument.csv"),
  show_col_types = FALSE
) |>
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
        "Market-based instruments",
        "Regulatory and \n redistributive instruments",
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
        "Egalitarianists", "Universalists", "Utilitarianists"
      ),
      labels = c(
        "Egalitarianists", "Universalists", "Utilitarianists"
      )
    )
  ) |>
  filter(level %in% c("Market-based instruments", "Regulatory and \n redistributive instruments"))


plot_policy_features <- function(data){
  ggplot(data,
         aes(
           x = estimate,
           y = level,
           colour = justice_class
         )) +
    geom_point(
      size = 2,
      position = position_dodge(width = 0.5)
    ) +
    geom_errorbarh(
      aes(
      xmax = upper,
      xmin = lower
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
    ) +
    theme_classic() +
    theme(
      legend.position = "right",
      text = element_text(size = 11),
      strip.background = element_rect(size = 0),
      strip.text.x = element_text(size = 11, face = "bold")
    )
}

plot_features_per_class <- function(data){
  ggplot(data,
         aes(
           x = estimate,
           y = level,
           colour = justice_class
         )) +
    geom_point(
      aes(size = justice_class),
      position = position_dodge(width = 0.5)
    ) +
    geom_errorbarh(
      aes(
        xmax = upper,
        xmin = lower
      ),
      height = .1,
      position = position_dodge(width = 0.5)
    ) +
    facet_wrap(
      vars(justice_class)
    ) +
    geom_vline(
      xintercept = .5,
      linetype = 2,
      colour = "gray40",
      size = .3
    ) +
    scale_color_viridis_d(
      end = .8,
      labels = c(
        "Egalitarianists (39.3%)",
        "Universalists (50.9%)",
        "Utilitarianists (9.8%)"
      ),
    ) +
    scale_size_manual(
      values = c(
        "Egalitarianists" = 2.5,
        "Universalists" = 3,
        "Utilitarianists" = 1
      ),
      labels = c(
        "Egalitarianists (39.3%)",
        "Universalists (50.9%)",
        "Utilitarianists (9.8%)"
      ),
      guide = guide_legend(title = NULL)
    ) +
    labs(
      colour = NULL,
      size = NULL,
      shape = NULL,
      y = NULL,
      x = "Marginal means"
    ) +
    theme_classic() +
    theme(
      legend.position = "right",
      text = element_text(size = 11),
      strip.background = element_rect(size = 0),
      strip.text.x = element_text(size = 11, face = "bold")
    )
}

push_plot <- df_push |>
  plot_policy_features() +
  scale_x_continuous(limits = c(.29, .71))

util_plot1 <- df_util |>
  filter(experiment == "Renewable energy scale-up") |>
  plot_features_per_class() +
  scale_x_continuous(limits = c(.29, .71))

util_plot2 <- df_util |>
  filter(experiment == "Heating sector decarbonisation") |>
  plot_features_per_class() +
  scale_x_continuous(limits = c(.29, .71)) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

util_plot <- util_plot1 / util_plot2 +
  plot_layout(guides = "collect", axis_titles = "collect") +
  plot_annotation(tag_levels = "A")

ggsave(
  plot = push_plot,
  here("output", "mm_stringency.png"),
  height = 4, width = 10
)

ggsave(
  plot = util_plot,
  here("output", "mm_dist_design.png"),
  height = 5, width = 10
)
