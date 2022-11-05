# R script for "Hvor mange vil stemme på Veganerpartiet? #2"
# URL: https://erikgahner.dk/2022/hvor-mange-vil-stemme-pa-veganerpartiet-2/

library("tidyverse")
library("lubridate")

polls <- read_csv("https://raw.githubusercontent.com/erikgahner/polls/master/polls.csv",
                  col_types = cols(
                    party_p = col_double(),
                    party_q = col_double(),
                    party_e = col_double(),
                    party_g = col_double(),
                    party_m = col_double(),
                    party_ae = col_double()
                  )) |> 
  mutate(date = make_date(year, month, day),
         n = ifelse(is.na(n), 1000, n))

polls |> 
  drop_na(party_aa, party_g) |> 
  mutate(ga = ifelse(date >= as.Date("2022-09-19"), "Grøn Alliance", "Veganerpartiet")) |> 
  ggplot(aes(party_g, party_aa, colour = ga)) +
  geom_jitter(width = 0.05, height = 0.05) +
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = 2, linetype = "dashed") +
  geom_vline(xintercept = 2, linetype = "dashed") +
  labs(x = "Opbakning til Veganerpartiet/Grøn Alliance (%)",
       y = "Opbakning til Alternativet (%)") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.text = element_text(face = "bold", color = "grey20"),
    axis.text = element_text(color = "grey40")
  ) +
  scale_colour_manual(values = c("#01FF70", "#3D9970"))

ggsave("veganerpartiet_alternativet_1.png", width = 6, height = 5, bg = "white")

polls |> 
  drop_na(party_aa, party_g) |> 
  mutate(party_aag = party_aa + party_g) |> 
  ggplot(aes(party_aag)) +
  geom_histogram(fill = "#3D9970", colour = "black", bins = 20) +
  geom_vline(xintercept = 2, linetype = "dashed") +
  labs(x = "Kombineret opbakning til Veganerpartiet/Grøn Alliance og Alternativet (%)",
       y = NULL) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.text = element_text(face = "bold", color = "grey20"),
    axis.text = element_text(color = "grey40")
  ) 
  
ggsave("veganerpartiet_alternativet_2.png", width = 7, height = 4, bg = "white")

