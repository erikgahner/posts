# R script for "Visualisering af støtte til forskellige coronarestriktioner"
# Link: https://erikgahner.dk/2021/visualisering-af-stotte-til-forskellige-coronarestriktioner/

library("tidyverse")

df <- data.frame(
  restriktion = c("Coronapas på offentlige steder",
                  "Coronapas på uddannelser og jobs",
                  "Mundbind i transport",
                  "Mundbind på offentlige steder",
                  "Nedlukning af skoler med meget smitte",
                  "Nedlukning af uddannelser",
                  "Tvunget hjemmearbejde i offentlige", # sic
                  "Nedlukning af alle arbejdspladser",
                  "Forsamlingsforbud på 100",
                  "Forsamlingsforbud på 500",
                  "Forsamlingsforbud på 1000"
                  ), 
  support = c(.92, .85, .76, .62, .75, .25, .30, .19, .45, .57, .64),
  usikkerhed = c(.03, .04, .05, .05, .05, .05, .05, .05, .06, .06, .05)
) 

df |> 
  mutate(restriktion = fct_reorder(restriktion, support)) |> 
  ggplot(aes(restriktion, support, ymin = support - usikkerhed, ymax = support + usikkerhed)) +
  geom_col() +
  geom_errorbar(width = 0) +
  coord_flip() +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .1), labels = scales::percent_format(accuracy = 5L)) +
  labs(title = "Opbakning til indførelsen af forskellige restriktioner (%)",
       x = NULL,
       y = NULL)

ggsave("covid_restriktioner_ggplot2.png", width = 7, height = 3)

