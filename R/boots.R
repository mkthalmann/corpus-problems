library(tidyverse)
library(here)
library(hrbrthemes)
library(patchwork)

set.seed(1234)
d <- tibble(
    V1 = sample(40:115, 50, replace = TRUE),
    V2 = sample(60:140, 50, replace = TRUE),
    id = factor(1:50)
) %>%
    mutate(
        id = glue::glue("t_{id}")
    ) %>%
    pivot_longer(
        cols = c(V1, V2),
        names_to = "condition",
        values_to = "frequency"
    )

source(here("R", "theme.R"))

p_50_boot <- d %>%
    ggplot(aes(x = condition, y = frequency, color = condition, fill = condition)) +
    stat_summary(fun = mean, geom = "bar", width = .8, alpha = .7, size = 1) +
    stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = .25, size = 1) +
    scale_color_manual(values = colors) +
    guides(fill = "none", color = "none") +
    scale_y_continuous(limits = c(0, 150), expand = c(0.001, 0)) +
    coord_cartesian(clip = "off") +
    labs(
        x = "",
        y = "Frequency \u00B1 CI<sub>*95%*</sub> Boot, 50 texts"
    ) +
    theme_mt(base_size = 15)


p_50_norm <- d %>%
    ggplot(aes(x = condition, y = frequency, color = condition, fill = condition)) +
    stat_summary(fun = mean, geom = "bar", width = .8, alpha = .7, size = 1) +
    stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = .25, size = 1) +
    scale_color_manual(values = colors) +
    guides(fill = "none", color = "none") +
    scale_y_continuous(limits = c(0, 150), expand = c(0.001, 0)) +
    coord_cartesian(clip = "off") +
    labs(
        x = "",
        y = "Frequency \u00B1 CI<sub>*95%*</sub>, 50 texts"
    ) +
    theme_mt(base_size = 15)



p_5_boot <- d %>%
    head(12) %>%
    ggplot(aes(x = condition, y = frequency, color = condition, fill = condition)) +
    stat_summary(fun = mean, geom = "bar", width = .8, alpha = .7, size = 1) +
    stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = .25, size = 1) +
    scale_color_manual(values = colors) +
    guides(fill = "none", color = "none") +
    scale_y_continuous(limits = c(0, 150), expand = c(0.001, 0)) +
    coord_cartesian(clip = "off") +
    labs(
        x = "",
        y = "Frequency \u00B1 CI<sub>*95%*</sub> Boot, 6 texts"
    ) +
    theme_mt(base_size = 15)


p_5_norm <- d %>%
    head(12) %>%
    ggplot(aes(x = condition, y = frequency, color = condition, fill = condition)) +
    stat_summary(fun = mean, geom = "bar", width = .8, alpha = .7, size = 1) +
    stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = .25, size = 1) +
    scale_color_manual(values = colors) +
    guides(fill = "none", color = "none") +
    scale_y_continuous(limits = c(0, 150), expand = c(0.001, 0)) +
    labs(
        x = "",
        y = "Frequency \u00B1 CI<sub>*95%*</sub>, 6 texts"
    ) +
    theme_mt(base_size = 15)

p_5_all <- d %>%
    head(12) %>%
    ggplot(aes(x = condition, y = frequency, color = condition, fill = condition)) +
    stat_summary(fun = mean, geom = "bar", width = .8, alpha = .7, size = 1) +
    scale_color_manual(values = colors) +
    guides(fill = "none", color = "none") +
    scale_y_continuous(limits = c(0, 150), expand = c(0.001, 0)) +
    facet_wrap(~ id) +
    labs(
        x = "",
        y = "Frequencies by text"
    ) +
    theme_mt(base_size = 15) +
    theme(
        strip.background = element_blank(),
        strip.text.x = element_blank()
    ) 

p_all <- p_50_norm + p_50_boot + p_5_norm + p_5_boot + p_5_all +
    plot_layout(ncol = 5, widths = c(1, 1, 1, 1, 1.8))

ggsave(
    here("media", "boots.pdf"),
    p_all,
    device = cairo_pdf,
    width = 15,
    height = 5.5,
    dpi = "retina",
    units = "in"
)

p_5_novar <- d %>%
    head(12) %>%
    ggplot(aes(x = condition, y = frequency, color = condition, fill = condition)) +
    stat_summary(fun = mean, geom = "bar", width = .8, alpha = .7, size = 1) +
    scale_color_manual(values = colors) +
    guides(fill = "none", color = "none") +
    scale_y_continuous(limits = c(0, 150), expand = c(0.001, 0)) +
    coord_cartesian(clip = "off") +
    labs(
        x = "",
        y = "Total frequency, 6 texts"
    ) +
    theme_mt(base_size = 24)

ggsave(
    here("media", "boots-novar.pdf"),
    p_5_novar,
    device = cairo_pdf,
    width = 12,
    height = 7.5,
    dpi = "retina",
    units = "in"
)
