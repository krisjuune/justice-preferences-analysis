library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(patchwork)
library(here)
source(here("functions", "r-assist.R"))

df_util_just <- read_csv(
  here("data", "mm_instrument.csv"),
  show_col_types = FALSE
)

df_util_gen <- read_csv(
  here("data", "mm_instrument_overall.csv"),
  show_col_types = FALSE
)

df_util <- bind_rows(
  df_util_just |> mutate(subgroups = "justice"),
  df_util_gen |> mutate(subgroups = "general")
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
      levels = c("pv", "heat"),
      labels = c("Renewable energy", "Heating sector")
    ),
    sample = case_when(
      subgroups == "justice" ~ as.character(justice_class),
      subgroups == "general" ~ "Overall"
    ),
    sample = factor(
      sample,
      levels = c("Egalitarianists", "Universalists", "Utilitarianists", "Overall"),
      labels = c(
        "Egalitarianists (39.3%)",
        "Universalists (50.9%)",
        "Utilitarianists (9.8%)",
        "Overall sample"
      )
    )
  ) |>
  filter(level %in% c(
    "Market-based instruments",
    "Regulatory and \n redistributive instruments"
  ))

plot_features_per_class <- function(data){
  ggplot(data,
         aes(
           x = estimate,
           y = level,
           colour = sample,
           shape = experiment
         )) +
    geom_vline(
      xintercept = .5,
      linetype = 2,
      colour = "gray40",
      size = .3
    ) +
    geom_point(
      aes(size = experiment),
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
      vars(sample),
      ncol = 4
    ) +
    scale_color_manual(
      values = my_palette(4, named = TRUE),
      guide = "none"
    ) +
    scale_shape_manual(
      values = c(
        "Renewable energy" = "circle",
        "Heating sector" = "diamond"
      ),
      name = "Experiment"
    )  +
    scale_size_manual(
      values = c(
        "Renewable energy" = 2,
        "Heating sector"   = 2.8
      ),
      name = "Experiment"
    ) +
    labs(
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

util_plot <- df_util |>
  plot_features_per_class() +
  scale_x_continuous(limits = c(.29, .71))

ggsave(
  plot = util_plot,
  here("output", "mm_dist_design.png"),
  height = 2.5, width = 10
)
