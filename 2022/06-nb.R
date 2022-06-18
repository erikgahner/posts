# R script for "Hvor mange vil stemme på Nye Borgerlige? #11"
# URL: https://erikgahner.dk/2022/hvor-mange-vil-stemme-pa-nye-borgerlige-11/

library("tidyverse")

Sys.setlocale("LC_TIME", "da_DK.UTF-8")
options(OutDec= ",")

polls_pr <- read_csv("prognose.csv")

nb <- polls_pr |> 
  filter(party_name == "Nye Borgerlige")

nb |> 
  ggplot(aes(dato, est, ymin = lower, ymax = upper)) +
  theme_minimal(base_size = 12) %+replace% 
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_line(colour = "grey20", size = 0.3, linetype="dotted"),
        panel.grid.minor.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none"
  ) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_line(colour = "#02505f", size = 0.8) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#02505f", alpha = .3) +
  labs(y = NULL,
       x = NULL,
       caption = "Opbakning til Nye Borgerlige, 2019-2022 (%)")

ggsave("nb.png", width = 7, height = 4, bg = "white")
