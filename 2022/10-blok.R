# R script for "Visualisering af politiske blokke i meningsmålingerne"
# URL: https://erikgahner.dk/2022/visualisering-af-politiske-blokke-i-meningsmalingerne/

library("tidyverse")
library("ggh4x")
library("colorspace")

blok_raw <- read_csv("blokModeraterne.csv")

blok_raw |> 
  filter(dato > as.Date("2022-08-01")) |> 
  select(-c("upper", "lower")) |> 
  filter(blok != "Moderaterne") |> 
  pivot_wider(names_from = "blok", values_from = "est") |> 
  ggplot(aes(x = dato)) +
  stat_difference(aes(ymin = Roed, ymax = Blaa), alpha = 0.3) +
  geom_line(aes(y = Roed, color = "Rød blok")) +
  geom_line(aes(y = Blaa, color = "Blå blok")) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(values = c("#3D85F7", "#C32E5A")) +
  scale_fill_manual(values = c(lighten("#3D85F7"), lighten("#C32E5A"), "white"), labels = c("Blåt flertal", "Rødt flertal", "")) +
  theme_minimal() +
  theme(
    legend.position = c(0.2, 0.12),
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.title = element_blank(),
    axis.title = element_blank(),
    strip.text = element_text(face = "bold", color = "grey20"),
    axis.text = element_text(color = "grey40"),
    plot.margin = margin(20, 30, 20, 30),
    plot.title = element_text(margin = margin(0, 0, -100, 0), size = 26, face = "bold", vjust = 0, color = "grey25"),
    plot.caption = element_text(size = 11)
  )

ggsave("blok_difference.png", width = 8, height = 4, bg = "white")
