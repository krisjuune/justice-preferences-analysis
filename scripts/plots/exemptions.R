library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(patchwork)
library(here)

main_text_size <- 10

df_plot <- read_csv(here("data", "mm_exemptions.csv")) |>
  filter(outcome == "choice")

df_plot <- df_plot |>
  filter(feature %in% c("tax", "ban")) |>
  filter(!level %in% c("25%", "75%")) |>
  mutate(
    level = factor(
      level,
      levels = c(
        "0%", "50%", "100%",
        "No ban", "Ban new installations", "Ban and replace fossil heating"
      ),
      labels = c(
        "0% tax increase", "50% tax increase", "100% tax increase",
        "Ban nothing", "Ban new installations", "Ban all fossil heating"
      )
    ),
    feature = factor(
      feature,
      levels = c(
        "ban", "tax"
      ),
      labels = c(
        "Ban", "Tax"
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
    ),
    exemption = factor(
      exemption,
      levels = c(
        "No exemptions",
        "Low-income exempted",
        "Low- and middle-income exempted"
      ),
      labels = c(
        "No exemptions",
        "Low-income exempted",
        "Low- and middle-income exempted"
      )
    )
  )

plot_exemptions <- function(data) {
  data |>
    ggplot(aes(
      x = estimate,
      y = level,
      colour = justice_class,
      shape = exemption
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
      vars(justice_class)
    ) +
    geom_vline(
      xintercept = .5,
      linetype = 2,
      colour = "gray40",
      linewidth = .3
    ) +
    scale_color_viridis_d(end = .8) +
    labs(
      colour = NULL,
      shape = "Exemptions design across justice profiles",
      y = NULL,
      x = "Marginal means"
    ) + 
    guides(color = "none")
}

exemption_ban_plot <- df_plot |>
  filter(feature == "Ban") |>
  plot_exemptions()

exemption_tax_plot <- df_plot |>
  filter(feature == "Tax") |>
  plot_exemptions()

theme_patchwork_justice <- function(plot) {
  plot +
  scale_x_continuous(limits = c(.25, .75)) +
  theme_classic() +
  theme(
    legend.position = "right",
    text = element_text(size = main_text_size),
    strip.background = element_rect(size = 0),
    strip.text.x = element_text(size = main_text_size, face = "bold")
  )
}

mm_exemptions_plot <- (exemption_ban_plot / exemption_tax_plot) +
  plot_layout(guides = "collect", axis_titles = "collect") +
  plot_annotation(tag_levels = "A") &
  scale_x_continuous(limits = c(.19, .81)) &
  theme_classic() &
  theme(
    legend.position = "right",
    text = element_text(size = main_text_size),
    strip.background = element_rect(size = 0),
    strip.text.x = element_text(size = main_text_size, face = "bold")
  )

ggsave(
  plot = mm_exemptions_plot,
  here("output", "mm_exemptions_plot.png"),
  height = 6, width = 10
)
