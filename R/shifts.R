library(tidyverse)
library(here)
library(hrbrthemes)

d_time <- tibble(
    DP = c(6.1 / 100, 15.2 / 100, 11.8 / 100),
    NP = c(28.9 / 100, 26.8 / 100, 30.8 / 100),
    stage = c("early", "intermediate", "late")
) %>%
    pivot_longer(
        cols = c(DP, NP),
        names_to = "condition",
        values_to = "percentage"
    )

source(here("R", "theme.R"))


p_shift_1 <- d_time %>%
    filter(stage != "late") %>%
    ggplot(
        aes(x = condition, y = percentage, color = stage, fill = stage)
    ) +
    geom_col(
        alpha = .7,
        position = position_dodge(width = .7),
        width = .5,
        size = 2
    ) +
    scale_y_percent() +
    geom_text(
        aes(label = glue::glue("{percentage*100}%"), y = percentage + 0.015),
        fontface = "bold",
        position = position_dodge(width = .7),
        size = 10,
    ) +
    scale_color_manual(values = colors) +
    coord_cartesian(clip = "off") +
    guides(color = "none", fill = "none") +
    labs(
        x = "Category",
        y = "Percentage"
    ) +
    theme_mt(base_size = 22)

ggsave(
    here("media", "shift-1.pdf"),
    p_shift_1,
    device = cairo_pdf,
    width = 20,
    height = 8,
    dpi = "retina",
    units = "in"
)

p_shift_2 <- d_time %>%
    ggplot(
        aes(x = condition, y = percentage, color = stage, fill = stage)
    ) +

    geom_col(
        alpha = .7,
        position = position_dodge(width = .7), width = .5, size = 2
    ) +
    scale_y_percent() +
    geom_text(
        aes(label = glue::glue("{percentage*100}%"), y = percentage + 0.015),
        fontface = "bold",
        position = position_dodge(width = .7),
        size = 10,
    ) +
    scale_fill_manual(values = colors2) +
    scale_color_manual(values = colors2) +
    coord_cartesian(clip = "off") +
    guides(color = "none", fill = "none") +
    labs(
        x = "Category",
        y = "Percentage"
    ) +
    theme_mt(base_size = 22)

ggsave(
    here("media", "shift-2.pdf"),
    p_shift_2,
    device = cairo_pdf,
    width = 20,
    height = 8,
    dpi = "retina",
    units = "in"
)
