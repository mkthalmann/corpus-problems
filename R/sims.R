library(tidyverse)
library(here)
library(hrbrthemes)
library(scattermore)

# # simulation
# alldiffs <- tibble()
# d_all <- tibble()
# for (samp1 in c(15, 30)) {
#     for (samp2 in c(15, 30, 12, 25)) {
#         for (diff in c(1, 1.1, 1.2, 1.4, 1.5)) {
#             d <- tibble()
#             set.seed(123456)
#             for (i in 1:10000) {
#                 a <- rpois(samp1, 10)
#                 b <- rpois(samp2, 10 * diff)
#                 temp <- broom::tidy(t.test(a, b))
#                 d <- rbind(d, temp)
#             }
#             d <- d %>%
#                 mutate(
#                     psig = if_else(
#                         p.value <= 0.05,
#                         "significant",
#                         "not significant"
#                     ),
#                     truediff = diff,
#                     sample1 = samp1,
#                     sample2 = samp2
#                 )
#             d_all <- rbind(d_all, d)
#             d <- d %>%
#                 count(psig) %>%
#                 mutate(
#                     truediff = diff,
#                     sample1 = samp1,
#                     sample2 = samp2
#                 )
#             alldiffs <- rbind(alldiffs, d)
#         }
#     }
# }

# d_all <- d_all %>%
#     select(-method)

# write_csv(alldiffs, here("R", "sim_summary.csv"), quote = "none")
# write_csv(d_all, here("R", "sim_all.csv"), quote = "none")

alldiffs <- read_csv(here("R", "sim_summary.csv"))
d_all <- read_csv(here("R", "sim_all.csv"))

alldiffs <- alldiffs %>%
    pivot_wider(
        names_from = psig,
        values_from = n
    ) %>%
    mutate(
        truediff = factor(truediff, ordered = T),
        sample = glue::glue("{sample1}/{sample2} Samples from Distribution"),
        sig_percent = significant / 10000,
        insig_percent = `not significant` / 10000
    )

source(here("R", "theme.R"))

p_sim <- ggplot(alldiffs, aes(
    y = sig_percent,
    x = truediff,
    color = "#066b8a",
    fill = "#066b8a"
)) +
    geom_col(alpha = .8) +
    facet_wrap(~sample, ncol = 4) +
    scale_color_manual(values = colors) +
    guides(color = "none", fill = "none") +
    scale_y_percent() +
    labs(
        x = "(True) Difference Factor (10 * X)",
        y = "Significant 2-Sample *t*-Tests (*N*<sub>*i*</sub> = 10,000)"
    ) +
    theme_mt(base_size = 15)

ggsave(
    here("media", "sim.pdf"),
    p_sim,
    device = cairo_pdf,
    width = 20,
    height = 8,
    dpi = "retina",
    units = "in"
)

p_sim_scatter <- d_all %>%
    mutate(
        truediff = factor(truediff, ordered = T),
        sample = glue::glue("{sample1}/{sample2} Samples from Distribution"),
    ) %>%
    ggplot(aes(x = truediff, y = p.value, color = psig)) +
    geom_scattermore(
        alpha = 0.1,
        pointsize = 1.5,
        position = position_jitter(width = 0.45, height = 0),
        interpolate = TRUE
    ) +
    facet_wrap(~sample, ncol = 4) +
    scale_color_manual(values = colors) +
    guides(color = "none") +
    scale_y_continuous(limits = c(0, 1), n.breaks = 17) +
    labs(
        x = "(True) Difference Factor (10 * X)",
        y = "*p* Value 2-Sample *t*-Tests (*N*<sub>*i*</sub> = 10,000)"
    ) +
    theme_mt(base_size = 22)


ggsave(
    here("media", "sim-scatter.pdf"),
    p_sim_scatter,
    device = cairo_pdf,
    width = 28,
    height = 10,
    dpi = "retina",
    units = "in"
)
