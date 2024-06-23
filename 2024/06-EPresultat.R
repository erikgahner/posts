# R script for "Hvordan klarede meningsmålingerne sig? #3"
# URL: https://erikgahner.dk/2024/hvordan-klarede-meningsmalingerne-sig-3/

library("tibble")
library("ggplot2")
library("tidyr")
library("dplyr")
library("stringr")
library("forcats")

Sys.setlocale("LC_TIME", "da_DK.UTF-8")

ep <- tribble(
    ~pollingfirm, ~date, ~respondenter, ~type, ~Socialdemokratiet, ~Radikale, ~Konservative, ~SF,
    ~LA, ~Moderaterne, ~DF, ~Venstre, ~Alternativet, ~Danmarksdemokraterne, ~Enhedslisten,
    # https://www.dr.dk/nyheder/politik/ep-valg/ny-meningsmaaling-fire-partier-er-paa-vippen-fire-dage-foer-ep-valget
    "Epinion", "2024-06-03", 2085, "Meningsmåling", 17.7, 4.9, 8.3, 16.6, 8.5, 6.0, 6.4, 10.8, 4.1, 8.9, 7.8,
    # https://www.berlingske.dk/politik/stort-drama-i-ny-maaling-mette-frederiksen-kan-maerke-en-aande-i-nakken
    "Verian", "2024-06-05", 2301, "Meningsmåling", 18.9, 5.4, 6.8, 16.6, 10.5, 5.9, 7.0, 11.5, 2.0, 8.8, 6.4,
    # https://nyheder.tv2.dk/politik/2024-06-08-sidste-maaling-inden-valget-spaar-gyser-mellem-to-partier
    "Megafon", "2024-06-07", NA_real_, "Meningsmåling", 17.9, 5.6, 8.2, 17.0, 10.0, 5.5, 6.6, 11.8, 2.4, 8.6, 6.4,
    # https://www.altinget.dk/eu/artikel/kun-faa-tusinde-stemmer-kan-flytte-afgoerende-eu-mandater
    "Altinget", "2024-06-07", NA_real_, "Vægtet snit", 18.9, 5.2, 7.4, 15.8, 9.9, 5.5, 7.1, 11.6, 2.6, 9.1, 6.9,
    # https://www.dr.dk/nyheder/politik/ep-valg/sf-staar-til-blive-stoerste-parti-i-exitpoll
    "Epinion", "2024-06-09", 2993, "Exit poll", 15.4, 6.9, 7.4, 18.4, 7.8, 6.2, 6.5, 13.9, 3.3, 7.6, 6.6,
    # https://nyheder.tv2.dk/politik/2024-05-31-doedt-loeb-mellem-radikale-og-moderaterne-viser-exitpoll
    "Megafon", "2024-06-09", 5800, "Exit poll", 18.0, 5.6, 8.2, 17.1, 9.7, 5.5, 7.0, 11.6, 2.4, 8.6, 6.3
)

ep_results <- ep |>
    pivot_longer(Socialdemokratiet:Enhedslisten, names_to = "parti", values_to = "support") |>
    mutate(resultat = case_when(parti == "Socialdemokratiet" ~ 15.6,
                                parti == "Radikale" ~ 7.1,
                                parti == "Konservative" ~ 8.8,
                                parti == "SF" ~ 17.4,
                                parti == "LA" ~ 7.0,
                                parti == "Moderaterne" ~ 5.9,
                                parti == "DF" ~ 6.4,
                                parti == "Venstre" ~ 14.7,
                                parti == "Alternativet" ~ 2.7,
                                parti == "Danmarksdemokraterne" ~ 7.4,
                                parti == "Enhedslisten" ~ 7.0)) |>
    mutate(forskel = abs(resultat - support),
           pollingfirmtype = str_glue("{pollingfirm} ({type})"))

ep_results |>
    ggplot(aes(x = fct_reorder(pollingfirmtype, type), y = support - resultat, fill = type)) +
    geom_col() +
    facet_wrap(~ parti, ncol = 3) +
    coord_flip() +
    scale_fill_manual(values = c("#FFC107", "#F44236", "#2196F3")) +
    theme_bw() +
    theme(legend.position = "none",
          axis.title = element_blank(),
          strip.text = element_text(color = "grey20"),
          axis.text = element_text(color = "grey40"),
          plot.margin = margin(20, 30, 20, 30),
          plot.title = element_text(margin = margin(0, 0, -100, 0), size = 26, vjust = 0, color = "grey25"),
          plot.caption = element_text(size = 11))

ggsave("ep_evaluering_01.png", width = 7, height = 6, bg = "white")

ep_results |>
    group_by(pollingfirmtype, type) |>
    summarise(fejl_AME = mean(abs(forskel)),
              fejl_RMSE = sqrt(mean((resultat - support)^2)),
              .groups = "drop") |>
    mutate(pollingfirmtype = fct_reorder(str_replace(pollingfirmtype, " ", "\n"), fejl_AME)) |>
    ggplot(aes(x = pollingfirmtype, y = fejl_AME, fill = type)) +
    geom_col() +
    labs(y = "Gennemsnitlig afvigelse i procentpoint",
         x = "") +
    scale_fill_manual(values = c("#FFC107", "#F44236", "#2196F3")) +
    theme_minimal() +
    theme(legend.position = "none",
        axis.title = element_blank(),
        strip.text = element_text(face = "bold", color = "grey20"),
        axis.text = element_text(color = "grey40"),
        plot.margin = margin(20, 30, 20, 30),
        plot.title = element_text(margin = margin(0, 0, -100, 0), size = 26, face = "bold", vjust = 0, color = "grey25"),
        plot.caption = element_text(size = 11)
    )

ggsave("ep_evaluering_02.png", width = 7, height = 3, bg = "white")
