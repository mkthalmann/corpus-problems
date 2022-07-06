library(tidyverse)
library(here)
library(patchwork)

# upset

library(UpSetR)
movies <- read.csv(system.file("extdata", "movies.csv", package = "UpSetR"), header = T, sep = ";")
p_up <- upset(
    movies,
    empty.intersections = "on",
    order.by = "freq",
    keep.order = FALSE,
    nintersects = NA,
    text.scale = 2.5,
    point.size = 5,
    set_size.numbers_size = 6,
    set_size.show = TRUE,
    shade.color = "#B2D2DEFF",
    main.bar.color = "#066b8a",
    matrix.color = "#B1B8CA",
    sets.bar.color = "#8a064a"
)

cairo_pdf(
    file = here("media", "upset.pdf"),
    width = 20,
    height = 8,
    family = "Roboto Condensed"
)
p_up
dev.off()

# keller and hampbusch 2008
d <- tibble(
    order = c(
        "S-I-O", "S-O-I", "I-S-O", "O-S-I", "I-O-S", "O-I-S", "Sp-O-I", "Sp-I-O", "I-Sp-O", "O-Sp-I", "I-O-Sp", "O-I-Sp", "S-Ip-O", "Ip-S-O", "S-O-Ip", "I-Ip-S", "Ip-O-S", "O-S-Ip", "S-Op-I", "Op-S-I", "S-I-Op", "Op-I-S", "I-S-Op", "I-Op-S"
    ),
    judgment = c(
        .2083, .0994, -.0716, -.2038, -.2667, -.2736, .1519, .1386, -.1463, -.2081, -.2936, -.3471, .1471, .1144, -.0516, -.2164, -.2612, -.2810, .1938, .1235, -.1876, -.2247, -.2694, -.3550
    ),
    corpus = c(
        54, 5, 0, 0, 0, 0, 4, 13, 0, 0, 0, 0, 30, 29, 0, 0, 0, 0, 3, 12, 0, 0, 0, 0
    )
)

source(here("R", "theme.R"))

p_exp <- d %>%
    ggplot(aes(x = fct_reorder(order, judgment), y = judgment, group = 1)) +
    geom_point(size = 5, color = "#066b8a") +
    geom_line(alpha = .5, color = "#066b8a") +
    labs(
        x = "Order",
        y = "Judgments (Keller 2000)",
    ) +
    coord_flip() +
    scale_y_continuous(n.breaks = 6)

p_corp <- d %>%
    ggplot(aes(x = fct_reorder(order, judgment), y = corpus, group = 1)) +
    geom_point(size = 5, color = "#066b8a") +
    geom_line(alpha = .5, color = "#066b8a") +
    labs(
        x = "",
        y = "Frequencies (Kempen & Harbusch 2005)",
    ) +
    coord_flip() +
    guides(y = "none") +
    scale_y_continuous(n.breaks = 6)

p_comb <- p_exp + p_corp

ggsave(
    here("media", "keller-kempen-harbusch.pdf"),
    p_comb,
    device = cairo_pdf,
    width = 15,
    height = 8.5,
    dpi = "retina",
    units = "in"
)


meklenborg <- tibble(
    century = factor(c("12^th", "12^th", "12^th", "12^th", "13^th", "13^th", "13^th", "13^th", "13^th", "14^th", "14^th", "14^th", "15^th", "15^th", "15^th"), ordered = T),
    estre = c(0, 0, 0, 0, 3, 0, 0, 0, 4, 6, 1, 2, 17, 6, 2),
    not_estre = c(4, 2, 0, 2, 4, 0, 0, 1, 2, 3, 1, 0, 0, 0, 0)
) %>%
    pivot_longer(
        cols = c(estre, not_estre), names_to = "cond", values_to = "freq"
    ) %>%
    mutate(
        cond = case_when(
            cond == "estre" ~ "*estre*",
            cond != "estre" ~ "not *estre*"
        )
    )

set.seed(12345)
p_m <- meklenborg %>%
    group_by(century, cond) %>%
    summarise(freq = sum(freq)) %>%
    ggplot(
        aes(
            x = century,
            y = freq,
            fill = cond,
            color = cond,
            group = cond,
            pch = cond,
            lty = cond
        )
    ) +
    geom_line() +
    geom_point(size = 8) +
    labs(
        x = "Century",
        y = "Meklenborg Nilsen (2021): Frequencies (*N*<sub>*total*</sub> = 60)",
        fill = "Verb",
        color = "Verb",
        pch = "Verb",
        lty = "Verb"
    ) +
    scale_color_manual(values = colors) +

meklenborg %>%
    ggplot(aes(x = century, y = freq, pch = cond, color = cond, group = cond, fill = cond)) +
    geom_jitter(height = 0, size = 5, alpha = .8, width = .3) +
    labs(
        x = "Century",
        y = "LM + Frequencies (*N*<sub>*texts*</sub> = 15)",
        fill = "Verb",
        color = "Verb",
        pch = "Verb",
        lty = "Verb"
    ) +
    scale_color_manual(values = colors) +
    coord_cartesian(ylim = c(0, 12)) +
    scale_x_discrete(expand = c(0.1, 0.2)) +
    scale_y_continuous(expand = c(0.001, 0.2), n.breaks = 5) +
    geom_smooth(method = "lm", alpha = .1)

ggsave(
    here("media", "meklenborg.pdf"),
    p_m,
    device = cairo_pdf,
    width = 18,
    height = 9.3,
    dpi = "retina",
    units = "in"
)
