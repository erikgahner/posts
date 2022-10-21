# R script for "Visualisering af meningsmålinger over tid"
# URL: https://erikgahner.dk/2022/visualisering-af-meningsmalinger-over-tid/

library("tidyverse")
library("lubridate")
library("ggforce")

polls_raw <- read_csv("https://raw.githubusercontent.com/erikgahner/polls/master/polls.csv",
                  col_types = cols(
                    party_p = col_double(),
                    party_q = col_double(),
                    party_e = col_double(),
                    party_g = col_double(),
                    party_m = col_double(),
                    party_ae = col_double()
                  ))


polls <- polls_raw |> 
  mutate(date = make_date(year, month, day),
         n = ifelse(is.na(n), 1000, n),
         across(starts_with("party"), ~ qbeta(1 - 0.05 / 2, n * (.x/100) + 1, n - n * (.x/100)) * 100, .names = "ci_max_{.col}"),
         across(starts_with("party"), ~ qbeta(0.05 / 2, n * (.x/100), n - n * (.x/100) + 1) * 100, .names = "ci_min_{.col}")
  )

polls_2019 <- polls |> 
  filter(date > as.Date("2019-06-05"))

polls_2019 |>
  pivot_longer(party_a:party_ae, names_to = "party", values_to = "support") |> 
  drop_na(support) |> 
  mutate(ci = 1.96 * sqrt((support * (100 - support)) / n)) |> 
  select(date, party, support, ci) |> 
  filter(party %in% c("party_a", "party_c", "party_v", "party_m")) %>%
  ggplot(aes(x=as.Date(date), y=support, ymin = support - ci, ymax = support + ci, colour=party)) +
  geom_point(size=1) +
  geom_errorbar(width = 0) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 2, linetype = "dashed") +
  labs(y = "Stemmer (%)",
       x = NULL,
       colour = NULL) +
  scale_colour_manual(labels = c("Socialdemokraterne", "Konservative", "Moderaterne", "Venstre"), 
                      values = c("#E3515D", "#429969", "#5bc3f5", "#459BC8")) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(panel.grid.major.y = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "grey80", color = NA),
        axis.title.x = element_text(hjust = 0),
        axis.title.y = element_text(hjust = 0),
        legend.title = element_text(face = "bold"),
        legend.position = "bottom") +
  facet_zoom(x = as.Date(date) > as.Date("2022-09-01")) 

ggsave("polls_zoom.png", width = 6, height = 8, bg = "white")
