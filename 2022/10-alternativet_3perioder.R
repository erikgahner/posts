# R script for "Hvordan klarer Alternativet sig i meningsmålingerne? #3"
# URL: https://erikgahner.dk/2022/hvordan-klarer-alternativet-sig-i-meningsmalingerne-3/

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
  filter(party_aa >= 2) |> 
  arrange(desc(date))

polls |> 
  drop_na(party_aa) |> 
  mutate(periode = case_when(
    year < 2017 ~ "Fremgang",
    year >= 2020 ~ "Overlevelse",
    TRUE ~ "Deroute"
  )) |> 
  ggplot(aes(date, party_aa)) +
  geom_point(colour = "black", size = 2.5) +
  geom_point(colour = "#2ECC40", size = 1, alpha = 0.8) +
  geom_hline(yintercept = 2, linetype = "dashed") +
  theme_minimal(base_family = "Roboto Condensed")  +
  theme(panel.grid.major.y = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(face = "bold", colour = "gray30"),
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "grey80", color = NA),
        legend.title = element_text(face = "bold"),
        legend.position = "none") +
  labs(y = NULL,
       x = NULL) +
  ggforce::geom_mark_ellipse(
    aes(fill = periode, label = periode), 
    alpha = 0, show.legend = FALSE
  ) +
  scale_y_continuous(limits = c(-0.5, 10.5), 
                     breaks = c(0, 2, 4, 6, 8, 10),
                     labels = c(0, 2, 4, 6, 8, "10%"))

ggsave("alternativet_3perioder.png", width = 8, height = 4, bg = "white")

