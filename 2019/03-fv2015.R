# R script to produce figure in "Boganmeldelse: Oprør fra udkanten"
# Link: http://erikgahner.dk/2019/boganmeldelse-opror-fra-udkanten/

# Load packages
library("tidyverse")
library("ggparliament")

party_names <- c("Enhedslisten", "Alternativet", "SF", "Radikale", "Social-\ndemokraterne", "Venstre", "Konservative", "Liberal Alliance", "DF")

dk2015 <- data.frame(
  party = factor(party_names),
  seats = c(14, 9, 7, 8, 47, 34, 6, 13, 37),
  colour = c("#731525", "#5AFF5A", "#9C1D2A", "#ED3D95", "#E32F3B", "#0F84BB", "#004E3A", "#EF8535", "#005078")
)

dk_semicircle <- parliament_data(election_data = dk2015,
                                       party_seats = dk2015$seats,
                                       parl_rows = 6,
                                       type = "semicircle")

ggplot(dk_semicircle, aes(x, y, colour = party)) +
  draw_majoritythreshold(n = 90, label = FALSE, type = "semicircle", linecolour="gray") + 
  geom_parliament_seats() + 
  theme_ggparliament() +
  scale_colour_manual(values = c("#731525", "#5AFF5A", "#9C1D2A", "#ED3D95", "#E32F3B", "#0F84BB", "#004E3A", "#EF8535", "#005078"), 
                      limits = party_names) +
  theme(legend.position = 'none') +
  draw_partylabels(type = "semicircle",
                 party_colours = colour,
                 party_names = party,
                 party_seats = seats) 

ggsave("fv2015.png", width = 10, height = 4)

