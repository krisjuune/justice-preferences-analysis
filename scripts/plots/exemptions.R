library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(patchwork)
library(here)

df_plot <- read_csv(here("data", "amce_exemptions.csv")) |>
  filter(outcome == "choice")

df_plot_tax_ban <- read_csv(here("data", "mm_exemptions_tax_ban.csv")) |>
  filter(outcome == "choice", !is.na(justice_class))

main_text_size <- 10

df_plot <- df_plot |>
  filter(outcome == "choice") |>
  mutate(
    push = factor(
      push,
      levels = c("Low stringency", "Medium stringency", "High stringency")
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
        "Overall"
      )
    ),
    level = factor(
      level,
      levels = c(
        "No exemptions",
        "Low-income exempted",
        "Low- and middle-income exempted"
      ),
      labels = c(
        "None",
        "Low-income",
        "Low- and middle-income"
      )
    )
  )

plot_exemptions <- function(data){
  ggplot(data,
         aes(
           x = estimate,
           y = level,
           colour = sample,
         )) +
    geom_point(
      size = 2.8,
      shape = "diamond",
      position = position_dodge(width = 0.5)
    ) +
    geom_errorbarh(
      aes(
        xmax = upper,
        xmin = lower
      ),
      height = .3,
      position = position_dodge(width = 0.5)
    ) +
    facet_grid(
      rows = vars(sample),
      cols = vars(push)
    )+
    geom_vline(
      xintercept = 0,
      linetype = 2,
      colour = "gray40",
      size = .3
    ) +
    scale_color_manual(
      values = c(
        "Egalitarianists (39.3%)" = viridis::viridis(3, end = .8)[1],
        "Universalists (50.9%)" = viridis::viridis(3, end = .8)[2],
        "Utilitarianists (9.8%)" = viridis::viridis(3, end = .8)[3],
        "Overall" = "gray50"
      ),
      name = "Justice orientation"
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
    ) +
    ggtitle("Exempted households")
}

plot_exemptions <- df_plot |>
  filter(push != "Low stringency") |>
  plot_exemptions() +
  theme(
    strip.background = element_blank(),
    strip.text.y = element_blank()
  )

ggsave(
  plot = plot_exemptions,
  here("output", "amce_exemptions_plot.png"),
  height = 7, width = 10
)

# also make the plot for tax and ban separately
df_plot_tax_ban <- df_plot_tax_ban |>
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
        "Egalitarianists", "Universalists", "Utilitarianists"
      ),
      labels = c(
        "Egalitarianists", "Universalists", "Utilitarianists"
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
        "No-one",
        "Low-income households",
        "Low- and middle-income households"
      )
    )
  )

plot_exemptions_policy <- function(data) {
  data |>
    ggplot(aes(
      x = estimate,
      y = level,
      colour = justice_class,
      shape = exemption,
      alpha = exemption
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
    scale_color_viridis_d(
      end = .8,
      guide = "none") +
    scale_alpha_manual(values = c(.5, 1, 1)) +
    labs(
      colour = NULL,
      shape = "Exemptions applied to",
      alpha = "Exemptions applied to",
      y = NULL,
      x = "Marginal means"
    )
}

exemption_ban_plot <- df_plot_tax_ban |>
  filter(feature == "Ban") |>
  plot_exemptions_policy()

exemption_tax_plot <- df_plot_tax_ban |>
  filter(feature == "Tax") |>
  plot_exemptions_policy()

mm_exemptions_tax_ban_plot <- (exemption_ban_plot / exemption_tax_plot) +
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
  plot = mm_exemptions_tax_ban_plot,
  here("output", "mm_exemptions_tax_ban_plot.png"),
  height = 6, width = 10
)
