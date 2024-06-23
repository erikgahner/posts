# R script for "Hvordan vil danskerne stemme til Europa-Parlamentsvalget? #3"
# URL: https://erikgahner.dk/2024/hvordan-vil-danskerne-stemme-til-europa-parlamentsvalget-3/

library("tibble")
library("ggplot2")
library("tidyr")
library("dplyr")

Sys.setlocale("LC_TIME", "da_DK.UTF-8")

ep_polls <- tribble(
    ~pollingfirm, ~date, ~respondenter, ~svar, ~Socialdemokratiet, ~Radikale, ~Konservative, ~SF,
    ~LA, ~Moderaterne, ~DF, ~Venstre, ~Alternativet, ~Danmarksdemokraterne, ~Enhedslisten,
    # https://www.altinget.dk/eu/artikel/foerste-eu-maaling-alle-partier-staar-til-en-plads-i-europa-parlamentet-undtagen-et
    "Epinion", "2024-01-31", 1051, 763, 22, 6, 6, 12, 10, 7, 7, 11, 1, 9, 7,
    # https://www.dr.dk/nyheder/politik/vaelgerne-straffer-venstre-i-ny-eu-maaling
    "Epinion", "2024-03-13", 2152, 1203, 24, 5, 5, 14, 9, 7, 7, 12, 3, 7, 7,
    # https://www.dr.dk/nyheder/politik/ep-valg/ny-maaling-sender-moderaterne-ud-af-eu-men-spidskandidat-tager-det-ikke-saa
    "Epinion", "2024-04-29", 1938, 1109, 20.1, 7.0, 6.8, 14.6, 12.0, 4.5, 6.5, 11.2, 2.3, 8.1, 6.9,
    # https://www.berlingske.dk/politik/danskerne-er-i-vildrede-udbryder-valgforskeren-tre-uger-foer-valget-er-alt
    "Verian", "2024-05-14", 1565, NA_real_, 18.6, 4.7, 6.8, 14.0, 12.2, 3.9, 9.6, 13.6, 1.2, 9.1, 6.2,
    # https://www.dr.dk/nyheder/politik/ny-maaling-tre-uger-foer-eu-valg-overrasker-valgforsker
    "Epinion", "2024-05-14", 2025, 1054, 21.4, 5.4, 7.1, 13.1, 9.9, 4.0, 7.4, 12.5, 1.7, 10.4, 6.8,
    # https://nyheder.tv2.dk/politik/2024-05-17-venstre-staar-til-kaempe-valglussing-men-vil-alligevel-aande-lettet-op-siger-redaktoer
    "Megafon", "2024-05-17", 1020, NA_real_, 21, 7, 7, 16, 11, 7, 5, 13, 1, 6, 6,
    # https://nyheder.tv2.dk/politik/2024-05-24-df-er-stoerre-end-radikale-venstre-i-ny-ep-maaling-men-partiet-staar-alligevel-til-nul-mandater
    "Megafon", "2024-05-24", NA_real_, NA_real_, 17.6, 4.5, 7.3, 18.1, 10.9, 7.5, 5.8, 12.9, 2.1, 6.1, 6.8,
    # https://nyheder.tv2.dk/politik/2024-05-31-danske-partier-i-taet-kamp-om-mandaterne-i-ny-maaling
    "Megafon", "2024-05-30", NA_real_, NA_real_, 19.4, 5.3, 7.6, 17.3, 10.5, 5.9, 5.1, 11.9, 2.0, 8.3, 6.5,
    # https://www.dr.dk/nyheder/politik/ep-valg/ny-meningsmaaling-fire-partier-er-paa-vippen-fire-dage-foer-ep-valget
    "Epinion", "2024-06-03", 2085, NA_real_, 17.7, 4.9, 8.3, 16.6, 8.5, 6.0, 6.4, 10.8, 4.1, 8.9, 7.8,
    # https://nyheder.tv2.dk/politik/2024-06-03-radikale-staar-til-at-ryge-ud-i-ny-maaling
    "Megafon", "2024-06-03", NA_real_, NA_real_, 18.3, 4.7, 8.4, 18.0, 12.2, 5.3, 5.9, 11.0, 2.5, 7.1, 6.2,
    # https://www.berlingske.dk/politik/stort-drama-i-ny-maaling-mette-frederiksen-kan-maerke-en-aande-i-nakken
    "Verian", "2024-06-05", 2301, NA_real_, 18.9, 5.4, 6.8, 16.6, 10.5, 5.9, 7.0, 11.5, 2.0, 8.8, 6.4,
    # https://nyheder.tv2.dk/politik/2024-06-06-ny-maaling-sender-eu-begejstret-parti-direkte-ud-i-kulden
    "Megafon", "2024-06-06", NA_real_, NA_real_, 18.1, 4.9, 8.5, 17.5, 10.5, 5.4, 6.5, 11.2, 2.8, 7.7, 6.9,
    # https://nyheder.tv2.dk/politik/2024-06-08-sidste-maaling-inden-valget-spaar-gyser-mellem-to-partier
    "Megafon", "2024-06-07", NA_real_, NA_real_, 17.9, 5.6, 8.2, 17.0, 10.0, 5.5, 6.6, 11.8, 2.4, 8.6, 6.4
)

polls <- ep_polls |>
    pivot_longer(Socialdemokratiet:Enhedslisten, names_to = "parti", values_to = "support") |>
    mutate(date = as.Date(date),
           support = support / 100) |>
    mutate(ci_max = qbeta(1 - 0.05 / 2, respondenter * support + 1, respondenter - respondenter * support),
           ci_min = qbeta(0.05 / 2, respondenter * support, respondenter - respondenter * support + 1))

polls |>
    ggplot(aes(date, support, ymin = ci_min, ymax = ci_max, colour = pollingfirm)) +
    geom_point() +
    geom_errorbar(width = 0) +
    geom_line(alpha = 0.4) +
    geom_hline(yintercept = 0) +
    facet_wrap(~ parti, ncol = 3) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_colour_manual(values = c("#D04A3C", "#223A63", "#EE8433")) +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(x = NULL,
         colour = NULL,
         y = NULL)

ggsave("EP2024polls.png", width = 7, height = 7, bg = "white")
