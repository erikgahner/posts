# R script for "Hvor mange vil stemme på Nye Borgerlige? #12"
# URL: https://erikgahner.dk/2024/hvor-mange-vil-stemme-pa-nye-borgerlige-12/

library("tidyverse")

polls_raw <- read_csv("polls.csv",
    col_types = cols(
        party_p = col_double(),
        party_q = col_double(),
        party_e = col_double(),
        party_g = col_double(),
        party_m = col_double(),
        party_ae = col_double()
    )
)

polls <- polls_raw |>
    mutate(
        date = make_date(year, month, day),
        n = ifelse(is.na(n), 1000, n),
        across(starts_with("party"), ~ qbeta(1 - 0.05 / 2, n * (.x / 100) + 1, n - n * (.x / 100)) * 100, .names = "ci_max_{.col}"),
        across(starts_with("party"), ~ qbeta(0.05 / 2, n * (.x / 100), n - n * (.x / 100) + 1) * 100, .names = "ci_min_{.col}")
    ) |>
    select(pollingfirm, date, party_d, ci_min_party_d, ci_max_party_d) |>
    drop_na(party_d) |>
    mutate(
        over10 = ifelse(ci_max_party_d > 10, "Ja", "Nej"),
        voxyougov = case_when(
            pollingfirm == "YouGov" ~ "YouGov",
            pollingfirm == "Voxmeter" ~ "Voxmeter",
            TRUE ~ "Andet"
        )
    )


polls |>
    ggplot(aes(date, party_d,
        ymin = ci_min_party_d,
        ymax = ci_max_party_d
    )) +
    geom_hline(yintercept = 2, linetype = "dashed") +
    geom_errorbar(colour = "gray25", alpha = 0.5) +
    geom_point(colour = "gray25", alpha = 0.5) +
    scale_x_continuous(
        breaks = as.Date(paste0(2016:2024, "-01-01")),
        labels = 2016:2024
    ) +
    theme_minimal() +
    theme(
        axis.title.y = element_text(hjust = 0),
        axis.title.x = element_text(hjust = 0),
        panel.grid.major = element_line(color = "gray95"),
        panel.grid.minor = element_line(color = "gray95")
    ) +
    labs(y = "Opbakning til Nye Borgerlige (%) →", x = NULL)

ggsave("nyeborgerlige_01.png", bg = "white", width = 8, height = 4)

polls |>
    ggplot(aes(date, party_d,
        ymin = ci_min_party_d,
        ymax = ci_max_party_d,
        colour = over10
    )) +
    geom_hline(yintercept = 2, linetype = "dashed") +
    geom_errorbar(alpha = 0.5) +
    geom_point(alpha = 0.5) +
    scale_colour_manual(values = c("gray25", "gray90")) +
    scale_x_continuous(
        breaks = as.Date(paste0(2016:2024, "-01-01")),
        labels = 2016:2024
    ) +
    theme_minimal() +
    theme(
        legend.position = "none",
        axis.title.y = element_text(hjust = 0),
        axis.title.x = element_text(hjust = 0),
        panel.grid.major = element_line(color = "gray95"),
        panel.grid.minor = element_line(color = "gray95")
    ) +
    labs(y = "Opbakning til Nye Borgerlige (%) →", x = NULL)

ggsave("nyeborgerlige_02.png", bg = "white", width = 8, height = 4)

polls |>
    ggplot(aes(date, party_d,
        ymin = ci_min_party_d,
        ymax = ci_max_party_d,
        colour = voxyougov
    )) +
    geom_hline(yintercept = 2, linetype = "dashed") +
    geom_errorbar(alpha = 0.5) +
    geom_point(alpha = 0.5) +
    scale_colour_manual(values = c("gray90", "#FF851B", "#B10DC9")) +
    scale_x_continuous(
        breaks = as.Date(paste0(2016:2024, "-01-01")),
        labels = 2016:2024
    ) +
    theme_minimal() +
    theme(
        legend.position = "none",
        axis.title.y = element_text(hjust = 0),
        axis.title.x = element_text(hjust = 0),
        panel.grid.major = element_line(color = "gray95"),
        panel.grid.minor = element_line(color = "gray95")
    ) +
    labs(y = "Opbakning til Nye Borgerlige (%) →", x = NULL)

ggsave("nyeborgerlige_03.png", bg = "white", width = 8, height = 4)
