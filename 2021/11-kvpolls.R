# R script for "Meningsmålinger og kommunalvalg #2"
# Link: https://erikgahner.dk/2021/meningsmalinger-og-kommunalvalg-2/

library("tidyverse")

Sys.setlocale("LC_TIME", "da_DK.UTF-8")
options(OutDec= ",")

polls_raw <- read_csv("https://raw.githubusercontent.com/erikgahner/kvpolls/master/kvpolls.csv")

polls <- polls_raw %>% 
  filter(dato > as.Date("2021-10-01"), dato < as.Date("2021-11-12")) |> 
  mutate(usikkerhed = 1.96 * sqrt((opbakning * (100 - opbakning)) / n)) |> 
  mutate(navn = paste0(kommune, " (", institut, ")")) |>
  mutate(navn = case_when(
    kommune == "Bornholm" & dato == "2021-10-14" ~ "Bornholm (Jysk Analyse, 14.10)",
    kommune == "Bornholm" & dato == "2021-11-11" ~ "Bornholm (Jysk Analyse, 11.11)",
    TRUE ~ navn
  )) |> 
  mutate(forskel_2017 = opbakning - valg_2017)

max(polls$dato)

polls |> 
  count(navn, parti) |> 
  arrange(-n)

plot_party <- function(party, farve){
  
  temp_fig <- polls |> 
    filter(parti == party) |> 
    drop_na(forskel_2017) |> 
    mutate(navn = fct_reorder(navn, forskel_2017)) |> 
    ggplot(aes(x = navn, y = forskel_2017, ymin = forskel_2017 - usikkerhed, ymax = forskel_2017 + usikkerhed)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point(colour = farve) +
    geom_errorbar(width = 0, colour = farve) +
    coord_flip() +
    labs(y = "Forskel i opbakning mellem meningsmåling og KV17 (%-point)",
         x = NULL,
         #caption = "Inkluderer meningsmålinger foretaget i perioden 1. oktober til 11. november 2021\nPositive værdier indikerer øget opbakning fra KV17 til i dag.",
         title = party
         ) +
    theme_minimal(base_size = 12, base_family = "Barlow") %+replace% 
    theme(plot.title = element_text(colour = farve, size = 16),
          panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(), 
          panel.grid.major.y = element_line(colour = "grey90", size = 0.2),
          panel.grid.minor.y = element_blank(),
          legend.justification = c(0, 0),
          legend.position = "bottom",
          plot.margin=unit(c(.5, .5, 1.5, .5),"cm"),
          axis.ticks.x = element_line(colour = "gray48"),
          axis.ticks.y = element_blank()
    ) +
    guides(col = guide_legend(ncol = 2), fill = "none")
  
  ggsave(paste0("kv2021_", str_remove_all(party, " "), ".png"), temp_fig, width = 7, height = 5, device = "png")
  
  }

plot_party("Konservative", "#429969")

plot_party("Socialdemokratiet", "#E3515D")

plot_party("Dansk Folkeparti", "#3D6F8D")

plot_party("Venstre", "#459BC8")

plot_party("Enhedslisten", "#914A4F")

plot_party("Radikale Venstre", "#EB4295")

plot_party("SF", "#9C1D2A")

plot_party("Nye Borgerlige", "#05454F")

plot_party("Liberal Alliance", "#EE9A5F")

plot_party("Alternativet", "darkgreen")

plot_party("Kristendemokraterne", "orange")

