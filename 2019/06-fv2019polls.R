# R script to figures in "Hvordan klarede meningsmålingerne sig?"
# Link: https://erikgahner.dk/2019/hvordan-klarede-meningsmalingerne-sig/

library("tidyverse")
library("ggrepel")

theme_set(
  theme_grey(base_size = 11.5) %+replace% 
    theme(
      plot.margin = unit(rep(0.5, 4), "cm"), plot.background = element_blank(), panel.background = element_blank(),
      panel.border = element_blank(), legend.background = element_rect(fill = "transparent", colour = NA),
      legend.key = element_rect(fill = "transparent", colour = NA), legend.title = element_blank(),
      strip.background = element_rect(fill = "transparent", colour = NA),
      panel.grid.major = element_line(linetype = "dotted", colour = "#757575", size = 0.3), panel.grid.minor = element_blank(),
      axis.ticks = element_blank(), axis.line = element_line(color = "#FFFFFF", size = 0.3),
      plot.title = element_text(size = 12, hjust = 0, margin = margin(b = 15)),
      plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(b = 5)),
      plot.caption = element_text(size = 10, colour = "#212121", margin = margin(t = 15)),
      axis.title = element_text(size = 11, face = "plain"), axis.text = element_text(size = 10, face = "plain"),
      legend.text = element_text(size = 10), strip.text = element_text(size = 12, face = "plain")
    )
)

fv <- read_csv("06-fv2019polls.csv")

fv_long <- pivot_longer(fv, Politologi:TV2, names_to = "Institut", values_to = "Estimat")

fv_long <- fv_long %>%
  mutate(Forskel = abs(Resultat - Estimat),
         Gruppe = case_when(
           Institut %in% c("Politologi", "Djøf", "Berlingske", "Ritzau", "Risbjerg") ~ "Vægtet snit",
           Institut %in% c("YouGov", "Megafon", "Gallup", "Voxmeter", "Epinion") ~ "Enkeltmåling",
           TRUE ~ "Exit poll"
         ), Navn = case_when(
           Institut == "TV2" ~ "TV 2",
           Institut == "Ritzau" ~ "Ritzau\nIndex",
           Institut == "Politologi" ~ "Politologi\nPrognose",
           Institut == "Risbjerg" ~ "Risbjerg\nsnittet",
           Institut == "Djøf" ~ "Djøf-\nIndekset",
           Institut == "BT1725" ~ "BT\n(17:25)",
           Institut == "BT1940" ~ "BT\n(19:40)",
           Institut == "Berlingske" ~ "Berlingske\nBarometer",
           TRUE ~ Institut
         ))

fv_est <- fv_long %>%
  group_by(Institut) %>%
  summarise(Fejl = mean(Forskel)) %>%
  mutate(Gruppe = case_when(
    Institut %in% c("Politologi", "Djøf", "Berlingske", "Ritzau", "Risbjerg") ~ "Vægtet snit",
    Institut %in% c("YouGov", "Megafon", "Gallup", "Voxmeter", "Epinion") ~ "Enkeltmåling",
    TRUE ~ "Exit poll"), 
    Navn = case_when(
      Institut == "TV2" ~ "TV 2",
      Institut == "Ritzau" ~ "Ritzau\nIndex",
      Institut == "Politologi" ~ "Politologi\nPrognose",
      Institut == "Risbjerg" ~ "Risbjerg\nsnittet",
      Institut == "Djøf" ~ "Djøf-\nIndekset",
      Institut == "BT1725" ~ "BT\n(17:25)",
      Institut == "BT1940" ~ "BT\n(19:40)",
      Institut == "Berlingske" ~ "Berlingske\nBarometer",
      TRUE ~ Institut)
  )

ggplot(fv_est, aes(x = fct_reorder(Navn, Fejl), y = Fejl, fill = Gruppe)) +
  geom_col() +
  labs(y = "Gennemsnitlig afvigelse i procentpoint",
       x = "") +
  scale_fill_manual(values = c("#FFC107", "#F44236", "#2196F3")) +
  theme(legend.position="bottom", legend.spacing.x = unit(.2, 'cm'))
ggsave("fv19_mae_bar.png", width = 10, height = 6)

cols <- c("V" = "#459BC8", 
          "A" = "#E3515D", 
          "O" = "#3D6F8D", 
          "F" = "#9C1D2A", 
          "C" = "#429969", 
          "B" = "#EB4295", 
          "Ø" = "#914A4F", 
          "I" = "#EE9A5F", 
          "D" = "#05454F", 
          "Å" = "#AEFEAF",
          "E" = "#537D7A",
          "K" = "#F4CE97",
          "P" = "#000000")

fv_long %>%
  filter(Gruppe == "Enkeltmåling") %>%
  ggplot(aes(x = Estimat, y = Resultat, colour = party)) +
  geom_point(size= 3) +
  geom_label_repel(aes(label = party),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  facet_wrap(~ Navn, ncol = 3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_colour_manual(values = cols) +
  theme(legend.position="none") + 
  labs(y = "Resultat (%)",
       x = "Estimat (%)")
ggsave("fv19_mae_enkelt.png", width = 7, height = 7)

