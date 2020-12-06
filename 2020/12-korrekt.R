# R script to figures in "Er meningsmålingerne i Danmark præcise?"
# Link: https://erikgahner.dk/2020/er-meningsmalingerne-i-danmark-praecise/

# Load packages
library("tidyverse")

polls <- read_csv("https://raw.githubusercontent.com/erikgahner/polls/master/polls.csv",
                  col_types = cols(
                    party_e = col_double(),
                    party_g = col_double(),
                    party_p = col_double()
                  ))

polls <- polls %>% 
  mutate(
    date = as.Date(format(as.Date(c(paste(year, month, day, sep="-")), by = "days")))
  ) %>% 
  arrange(date) 

for(i in c("a", "b", "c", "d", "e", "f", "g", "i", "k", "o", "p", "v", "oe", "aa")) {
  polls <- within(polls, {
    assign(paste0("ci_", i), 1.96 * sqrt(( get(paste0("party_", i)) * (100 - get(paste0("party_", i)))) / n))
  }
  )
}

polls_2011 <- polls[polls$date > seq(as.Date("2011-09-15"), length = 2, by = "-7 days")[2] & polls$date < as.Date("2011-09-15"),]
korrekt_2011 <- polls_2011 %>% 
  mutate(korrekt_a = case_when(party_a + ci_a < 24.9 & party_a - ci_a < 24.9 ~ 0, TRUE ~ 1),
         korrekt_v = case_when(party_v + ci_v < 26.7 & party_v - ci_v < 26.7 ~ 0, TRUE ~ 1),
         korrekt_o = case_when(party_o + ci_o < 12.3 & party_o - ci_o < 12.3 ~ 0, TRUE ~ 1),
         korrekt_b = case_when(party_b + ci_b < 9.5 & party_b - ci_b < 9.5 ~ 0, TRUE ~ 1),
         korrekt_f = case_when(party_f + ci_f < 9.2 & party_f - ci_f < 9.2 ~ 0, TRUE ~ 1),
         korrekt_oe = case_when(party_oe + ci_oe < 6.7 & party_oe - ci_oe < 6.7 ~ 0, TRUE ~ 1),
         korrekt_i = case_when(party_i + ci_i < 5.0 & party_i - ci_i < 5.0 ~ 0, TRUE ~ 1),
         korrekt_c = case_when(party_c + ci_c < 5.0 & party_c - ci_c < 5.0 ~ 0, TRUE ~ 1)
         ) %>% 
  pivot_longer(starts_with("korrekt"), names_to = "parti", values_to = "korrekt")


polls_2015 <- polls[polls$date > seq(as.Date("2015-06-18"), length = 2, by = "-7 days")[2] & polls$date < as.Date("2015-06-18"),]

korrekt_2015 <- polls_2015 %>% 
  mutate(korrekt_a = case_when(party_a + ci_a < 26.3 & party_a - ci_a < 26.3 ~ 0, TRUE ~ 1),
         korrekt_o = case_when(party_o + ci_o < 21.1 & party_o - ci_o < 21.1 ~ 0, TRUE ~ 1),
         korrekt_v = case_when(party_v + ci_v < 19.5 & party_v - ci_v < 19.5 ~ 0, TRUE ~ 1),
         korrekt_oe = case_when(party_oe + ci_oe < 7.8 & party_oe - ci_oe < 7.8 ~ 0, TRUE ~ 1),
         korrekt_i = case_when(party_i + ci_i < 7.5 & party_i - ci_i < 7.5 ~ 0, TRUE ~ 1),
         korrekt_aa = case_when(party_aa + ci_aa < 4.8 & party_aa - ci_aa < 4.8 ~ 0, TRUE ~ 1),
         korrekt_b = case_when(party_b + ci_b < 4.6 & party_b - ci_b < 4.6 ~ 0, TRUE ~ 1),
         korrekt_f = case_when(party_f + ci_f < 4.2 & party_f - ci_f < 4.2 ~ 0, TRUE ~ 1),
         korrekt_c = case_when(party_c + ci_c < 3.4 & party_c - ci_c < 3.4 ~ 0, TRUE ~ 1)
  ) %>% 
  pivot_longer(starts_with("korrekt"), names_to = "parti", values_to = "korrekt") 


polls_2019 <- polls[polls$date > seq(as.Date("2019-06-05"), length = 2, by = "-7 days")[2] & polls$date < as.Date("2019-06-05"),]

korrekt_2019 <- polls_2019 %>% 
  mutate(korrekt_a = case_when(party_a + ci_a < 25.9 & party_a - ci_a < 25.9 ~ 0, TRUE ~ 1),
         korrekt_v = case_when(party_v + ci_v < 23.4 & party_v - ci_v < 23.4 ~ 0, TRUE ~ 1),
         korrekt_o = case_when(party_o + ci_o < 8.7 & party_o - ci_o < 8.7 ~ 0, TRUE ~ 1),
         korrekt_b = case_when(party_b + ci_b < 8.6 & party_b - ci_b < 8.6 ~ 0, TRUE ~ 1),
         korrekt_f = case_when(party_f + ci_f < 7.7 & party_f - ci_f < 7.7 ~ 0, TRUE ~ 1),
         korrekt_oe = case_when(party_oe + ci_oe < 6.9 & party_oe - ci_oe < 6.9 ~ 0, TRUE ~ 1),
         korrekt_c = case_when(party_c + ci_c < 6.6 & party_c - ci_c < 6.6 ~ 0, TRUE ~ 1),
         korrekt_aa = case_when(party_aa + ci_aa < 3.0 & party_aa - ci_aa < 3.0 ~ 0, TRUE ~ 1),
         korrekt_d = case_when(party_d + ci_d < 2.4 & party_d - ci_d < 2.4 ~ 0, TRUE ~ 1),
         korrekt_i = case_when(party_i + ci_i < 2.3 & party_i - ci_i < 2.3 ~ 0, TRUE ~ 1)
  ) %>% 
  pivot_longer(starts_with("korrekt"), names_to = "parti", values_to = "korrekt")


korrekt <- bind_rows(korrekt_2011, korrekt_2015, korrekt_2019) %>% 
  select(pollingfirm, year, parti, korrekt) %>% 
  mutate(parti_navn = case_when(
    parti == "korrekt_a" ~ "Socialdemokratiet",
    parti == "korrekt_v" ~ "Venstre",
    parti == "korrekt_o" ~ "Dansk Folkeparti",
    parti == "korrekt_b" ~ "Radikale",
    parti == "korrekt_oe" ~ "Enhedslisten",
    parti == "korrekt_i" ~ "Liberal Alliance",
    parti == "korrekt_f" ~ "SF",
    parti == "korrekt_aa" ~ "Alternativet",
    parti == "korrekt_d" ~ "Nye Borgerlige",
    parti == "korrekt_c" ~ "Konservative"
  ))

mean(korrekt$korrekt)

korrekt %>% 
  group_by(year) %>% 
  summarise(korrekt = mean(korrekt), .groups = "drop")

korrekt %>% 
  group_by(pollingfirm, year) %>% 
  summarise(korrekt = mean(korrekt) * 100, .groups = "drop") %>% 
  arrange(desc(korrekt)) %>% 
  print(n = NROW(.))

korrekt %>% 
  group_by(parti, year) %>% 
  summarise(korrekt = mean(korrekt) * 100, .groups = "drop") %>% 
  arrange(desc(korrekt)) %>% 
  print(n = NROW(.))


korrekt %>% 
  group_by(pollingfirm, year) %>% 
  summarise(korrekt = mean(korrekt), .groups = "drop") %>% 
  ggplot(aes(pollingfirm, korrekt)) +
  geom_segment(aes(xend = pollingfirm, yend = 0)) +
  geom_point(size=2) +
  facet_wrap(~ year) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_bw() +
  labs(y = "Korrekt (%)",
       x = NULL)
  
ggsave("korrekt_institut.png", width = 7, height = 3)


korrekt %>% 
  group_by(parti_navn, year) %>% 
  summarise(korrekt = mean(korrekt), .groups = "drop") %>% 
  mutate(korrekt_group = case_when(
    korrekt > .95 ~ "God",
    korrekt > .80 ~ "Middel",
    TRUE ~ "Dårlig")) %>% 
  ggplot(aes(parti_navn, korrekt, colour = korrekt_group)) +
  geom_segment(aes(xend = parti_navn, yend = 0)) +
  geom_point(size=2) +
  facet_wrap(~ year) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_colour_manual(values = c("#FF4136", "#2ECC40", "#FF851B")) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(y = "Korrekt (%)",
       x = NULL)
  
ggsave("korrekt_parti.png", width = 7, height = 3)