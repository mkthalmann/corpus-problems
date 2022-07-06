library(tidyverse)
library(here)
library(hrbrthemes)
library(patchwork)

source(here("R", "theme.R"))

d_judg <- tibble(
    judgment = c(54.9, 58.8, 33.6, 42.6),
    se = c(4.84, 4.42, 3.93, 3.68),
    vtype = c("canonical", "experiencer", "canonical", "experiencer"),
    context = c("licensing", "licensing", "non-licensing", "non-licensing")
)

d_choices <- tibble(
    prop_os = c(57.03, 69.53, 19.53, 41.41),
    vtype = c("canonical", "experiencer", "canonical", "experiencer"),
    context = c("licensing", "licensing", "non-licensing", "non-licensing")
)

p_judg <- ggplot(d_judg, aes(
    y = judgment,
    x = context,
    color = vtype,
    group = vtype
)) +
    geom_point(size = 6) +
    geom_line() +
    guides(color = "none") +
    scale_color_manual(values = colors) +
    coord_cartesian(ylim = c(0, 100))  +
    labs(
        x = "Context",
        y = "Mean Judgments"
    ) +
    theme_mt(base_size = 22)


p_choices <- ggplot(d_choices, aes(
    y = prop_os,
    x = context,
    color = vtype,
    group = vtype
)) +
    geom_point(size = 6) +
    geom_line() +
    guides(color = "none") +
    scale_color_manual(values = colors) +
    coord_cartesian(ylim = c(0, 100)) +
    labs(
        x = "Context",
        y = "Proportion of OS"
    ) +
    theme_mt(base_size = 22)

p_both <- p_judg + p_choices

ggsave(
    here("media", "effects.pdf"),
    p_both,
    device = cairo_pdf,
    width = 15,
    height = 6,
    dpi = "retina",
    units = "in"
)
