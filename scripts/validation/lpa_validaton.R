library(nnet)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(patchwork)
library(grid)
library(Matrix)
library(here)
library(cregg)

source(here("functions", "r-assist.R"))
source(here("scripts", "plots", "lpa_plots.R"))
source(here("scripts", "plots", "exemptions.R"))

my_palette_g4 <- function(n, named = FALSE) {
  colors_vec <- c(
    "#c6461f",
    "#d6775d",
    "#007b97",
    "#f59a00"
  )

  if (n == 3) {
    return(colors_vec[1:3])
  } else if (n == 4) {
    return(colors_vec)
  } else {
    stop("This palette supports only 3 or 4 colors.")
  }
}

set.seed(100)
main_text_size <- 10

data_g3 <- read_csv(
  here("data", "lpa_data.csv"),
  show_col_types = FALSE
) |>
  drop_na(justice_class) |>
  mutate(
    justice_class = factor(
      justice_class,
      levels = c(
        "1", "3", "2"
      ),
      labels = c(
        "1 (Egalitarians, 39%)", "2 (Universalists, 51%)", "3 (Utilitarians, 10%)"
      )
    )
  )

data_g4 <- read_csv(
  here("data", "lpa_data_g4.csv"),
  show_col_types = FALSE
) |>
  drop_na(justice_class) |>
  mutate(
    justice_class = factor(
      justice_class,
      levels = c(
        "2", "1", "4", "3"
      ),
      labels = c(
        "1 (Egalitarians type A, 11%)", "2 (Egalitarians type B, 28%)", "3 (Universalists, 51%)", "4 (Utilitarians, 10%)"
      )
    )
  )


########################## individual profiles #########################
class_proportions_g3 <- as.list(prop.table(table(data_g3$justice_class)))
class_proportions_g4 <- as.list(prop.table(table(data_g4$justice_class)))

plot_participant_profiles <- function(data, show_legend = FALSE) {
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
      y = "Sum score",
      color = NULL
    ) +
    theme_classic() +
    theme(
      legend.position = if (show_legend) "right" else "none",
      text = element_text(size = main_text_size),
      strip.background = element_rect(size = 0),
      strip.text.x = element_text(size = main_text_size, face = "bold")
    )

  plot
}

plot_3class <- data_g3 |>
  group_by(justice_class) |>
  slice_sample(n = 50) |>
  pivot_participant_profiles_long(shorten_labels = TRUE, get_z = FALSE) |>
  plot_participant_profiles(show_legend = TRUE) +
  scale_color_manual(values = my_palette(3)) +
  facet_wrap(~sub("^(\\d+).*", "\\1", justice_class), ncol = 3) +
  xlab(NULL) +
  ggtitle("3-group solution")

plot_4class <- data_g4 |>
  group_by(justice_class) |>
  slice_sample(n = 50) |>
  pivot_participant_profiles_long(shorten_labels = TRUE, get_z = FALSE) |>
  plot_participant_profiles(show_legend = TRUE) +
  scale_color_manual(values = my_palette_g4(4)) +
  facet_wrap(~sub("^(\\d+).*", "\\1", justice_class), ncol = 4) +
  ggtitle("4-group solution")

combined_profiles_plot <- plot_3class / plot_4class

ggsave(
  here("output", "lpa_validation_profiles.png"),
  plot = combined_profiles_plot,
  height = 6, width = 10
)

######################### policy application ##########################
df_heat <- read_csv(
  here("data", "heat_conjoint.csv"),
  show_col_types = FALSE
) |>
  filter_respondents() |>
  rename(temp_justice_class = justice_class) |>
  factor_conjoint(experiment = "heat") |>
  mutate(
    justice_class = factor(
      temp_justice_class,
      levels = c(
        "1", "3", "2"
      ),
      labels = c(
        "1 (Egalitarians, 39%)",
        "2 (Universalists, 51%)",
        "3 (Utilitarians, 10%)"
      )
    )
  )

as.list(prop.table(table(df_heat$justice_class)))

df_heat_g4 <- read_csv(
  here("data", "heat_g4_conjoint.csv"),
  show_col_types = FALSE
) |>
  filter_respondents() |>
  rename(temp_justice_class = justice_class) |>
  factor_conjoint(experiment = "heat") |>
  mutate(
    justice_class = factor(
      temp_justice_class,
      levels = c("2", "1", "4", "3"),
      labels = c(
        "1 (Egalitarians type A, 11%)",
        "2 (Egalitarians type B, 28%)",
        "3 (Universalists, 51%)",
        "4 (Utilitarians, 10%)"
      )
    )
  )

as.list(prop.table(table(df_heat_g4$justice_class)))

# get MMs
mm_exemptions_g3 <- cj(
  df_heat,
  Y ~ ban,
  id = ~id,
  estimate = "mm",
  by = ~exemption + justice_class
)

mm_exemptions_g4 <- cj(
  df_heat_g4,
  Y ~ ban,
  id = ~id,
  estimate = "mm",
  by = ~exemption + justice_class
)

plot_ban_g3 <- mm_exemptions_g3 |>
  plot_exemptions_policy() +
  scale_color_manual(values = my_palette(3), guide = "none") +
  ggtitle("3-group solution")

plot_ban_g4 <- mm_exemptions_g4 |>
  plot_exemptions_policy() +
  scale_color_manual(values = my_palette_g4(4), guide = "none") +
  ggtitle("4-group solution")

exemptions_plot <- (plot_ban_g3 / plot_ban_g4) +
  plot_layout(guides = "collect", axis_titles = "collect") &
  scale_x_continuous(limits = c(.2, .8)) &
  theme_classic() &
  theme(
    legend.position = "right",
    text = element_text(size = main_text_size),
    strip.background = element_rect(size = 0),
    strip.text.x = element_text(size = main_text_size, face = "bold")
  )

ggsave(
  plot = exemptions_plot,
  here("output", "lpa_validation_exemptions.png"),
  height = 7, width = 10
)
