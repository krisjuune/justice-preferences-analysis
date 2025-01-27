library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(patchwork)
library(here)

# Exemptions

df_plot <- read_csv(here("data", "heat_justice_class_MMs.csv")) |>
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
    BY = factor(
      BY,
      levels = c(
        "egalitarian", "universal", "utilitarian"
      ),
      labels = c(
        "Egalitarian", "Universal", "Utilitarian"
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
    )
  )

dummy_df1 <- df_plot |>
  mutate(
    estimate = estimate * .9,
    std.error = std.error * 2,
    exemptions = "No exemptions",
  )

dummy_df2 <- df_plot |>
  mutate(
    std.error = std.error * 2,
    exemptions = "Low income exempted"
  )

dummy_df3 <- df_plot |>
  mutate(
    estimate = estimate * 1.1,
    std.error = std.error * 2,
    exemptions = "Low and mid income exempted"
  )

df_plot <- bind_rows(dummy_df1, dummy_df2, dummy_df3) |>
  mutate(exemptions = factor(
    exemptions,
    levels = c("No exemptions", "Low income exempted", "Low and mid income exempted")
    ))


plot_exemptions <- function(data) {
  data |>
    ggplot(aes(
      x = estimate,
      y = level,
      colour = BY,
      shape = BY
    )) +
    geom_point(
      size = 2,
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
      vars(exemptions)
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
    text = element_text(size = 11),
    strip.background = element_rect(size = 0),
    strip.text.x = element_text(size = 11, face = "bold")
  )
  # ggplot2::theme(
  #   text = element_text(size = 14),
  #   strip.background = element_blank(),
  #   strip.text.x = element_text(size = 12.5, face = "bold"),
  #   panel.border = element_blank(),
  #   panel.background = element_blank(),
  #   panel.grid.major = element_blank(),
  #   panel.grid.minor = element_blank(),
  #   axis.line = element_line(colour = "black", linewidth = rel(1)),
  #   legend.text = element_text(size = 10),
  #   legend.background = element_blank(),
  # )
}


mm_exemptions_plot <- (exemption_ban_plot / exemption_tax_plot) +
  plot_layout(guides = "collect", axis_titles = "collect") +
  plot_annotation(tag_levels = "A") &
  scale_x_continuous(limits = c(.28, .72)) &
  theme_classic() &
  theme(
    legend.position = "right",
    text = element_text(size = 11),
    strip.background = element_rect(size = 0),
    strip.text.x = element_text(size = 11, face = "bold")
  )

ggsave(
  plot = mm_exemptions_plot,
  here("output", "mm_exemptions_plot.png"),
  height = 7, width = 11
)


