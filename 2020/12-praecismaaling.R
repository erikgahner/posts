# R script to figure in "Hvorfor skal meningsmålinger være så præcise? #2"
# Link: https://erikgahner.dk/2020/hvorfor-skal-meningsmalinger-vaere-sa-praecise-2/

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

polls %>% 
  filter(date == as.Date("2020-12-06"), pollingfirm == "Voxmeter") %>% 
  pivot_longer(party_a:party_aa, names_to = "parti") %>% 
  mutate(parti_navn = case_when(
    parti == "party_a" ~ "Socialdemokratiet",
    parti == "party_v" ~ "Venstre",
    parti == "party_o" ~ "Dansk Folkeparti",
    parti == "party_b" ~ "Radikale",
    parti == "party_oe" ~ "Enhedslisten",
    parti == "party_i" ~ "Liberal Alliance",
    parti == "party_k" ~ "Kristendemokraterne",
    parti == "party_f" ~ "SF",
    parti == "party_g" ~ "Veganerpartiet",
    parti == "party_aa" ~ "Alternativet",
    parti == "party_d" ~ "Nye Borgerlige",
    parti == "party_c" ~ "Konservative"
  )) %>% 
  drop_na(value) %>% 
  mutate(parti_navn = fct_reorder(parti_navn, value)) %>% 
  ggplot(aes(parti_navn, value)) +
  geom_point() +
  geom_errorbar(aes(ymin = value - 1.96 + sqrt(value * (100 - value)) / n, ymax = value + 1.96 + sqrt(value * (100 - value)) / n), width = 0, size = 0.5) +
  geom_errorbar(aes(ymin = value - 1.645 + sqrt(value * (100 - value)) / n, ymax = value + 1.645 + sqrt(value * (100 - value)) / n), width = 0, size = 1) +
  coord_flip() +
  theme_minimal() +
  labs(x = NULL,
       y = "Opbakning (%)")

ggsave("meningsmaaling_90_95.png", width = 8, height = 4)

