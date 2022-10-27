# R script for "Venstre og andre partier i meningsmålingerne"
# URL: https://erikgahner.dk/2022/venstre-og-andre-partier-i-meningsmalingerne/

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
         n = ifelse(is.na(n), 1000, n)) |> 
  mutate(valgperiode = case_when(
    date > as.Date("2011-09-15") & date <= as.Date("2015-06-18") ~ "2011-15",
    date > as.Date("2015-06-18") & date <= as.Date("2019-06-05") ~ "2015-19",
    date > as.Date("2019-06-05") ~ "2019-",
    TRUE ~ "Other")) |>
  mutate(distance = case_when(
    valgperiode == "2011-15" ~ as.numeric(difftime(as.Date(date),  as.Date("2011-09-15"), units = "days")),
    valgperiode == "2015-19" ~ as.numeric(difftime(as.Date(date),  as.Date("2015-06-18"), units = "days")),
    valgperiode == "2019-" ~ as.numeric(difftime(as.Date(date),  as.Date("2019-06-05"), units = "days"))
  )) |> 
  filter(valgperiode != "Other") 



polls |> 
  ggplot(aes(party_v, party_c)) +
  #forsøg evt. med density
  ##stat_density_2d(aes(fill = after_stat(density)), geom = "raster", contour = FALSE) +
  ##scale_fill_distiller(palette = "Blues", direction = 1) +
  geom_point(aes(alpha = distance^3), colour = "#429969") +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(face = "bold", colour = "gray30"),
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "grey90", color = NA),
        legend.title = element_text(face = "bold"),
        legend.position = "none") +
  facet_wrap(~ valgperiode) +
  labs(x = "Venstre",
       y = "Konservative")

ggsave("Venstre_C.png", width = 7, height = 3, bg = "white")

polls |> 
  ggplot(aes(party_v, party_a)) +
  geom_point(aes(alpha = distance^3), colour = "#E3515D") +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(face = "bold", colour = "gray30"),
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "grey90", color = NA),
        legend.title = element_text(face = "bold"),
        legend.position = "none") +
  facet_wrap(~ valgperiode) +
  labs(x = "Venstre",
       y = "Socialdemokraterne")

ggsave("Venstre_A.png", width = 7, height = 3, bg = "white")

polls |> 
  ggplot(aes(party_v, party_o)) +
  geom_point(aes(alpha = distance^3), colour = "#3D6F8D") +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(face = "bold", colour = "gray30"),
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "grey90", color = NA),
        legend.title = element_text(face = "bold"),
        legend.position = "none") +
  facet_wrap(~ valgperiode) +
  labs(x = "Venstre",
       y = "Dansk Folkeparti")

ggsave("Venstre_O.png", width = 7, height = 3, bg = "white")

polls |> 
  ggplot(aes(party_v, party_i)) +
  geom_point(aes(alpha = distance^3), colour = "#EE9A5F") +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(face = "bold", colour = "gray30"),
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "grey90", color = NA),
        legend.title = element_text(face = "bold"),
        legend.position = "none") +
  facet_wrap(~ valgperiode) +
  labs(x = "Venstre",
       y = "Liberal Alliance")

ggsave("Venstre_I.png", width = 7, height = 3, bg = "white")
