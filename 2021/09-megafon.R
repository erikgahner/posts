# R script for "Er Socialdemokratiet gået tilbage i meningsmålingerne? #4"
# Link: https://erikgahner.dk/2021/er-socialdemokratiet-gaet-tilbage-i-meningsmalingerne-4/

library("tidyverse")

polls <- read_csv("https://raw.githubusercontent.com/erikgahner/polls/master/polls.csv",
                  col_types = cols(
                    party_p = col_double(),
                    party_e = col_double(),
                    party_g = col_double()
                  ))

megafon <- polls |> 
  filter(pollingfirm == "Megafon") |> 
  filter(year == 2021, month %in% c(5, 8)) |> 
  mutate(maaling = ifelse(id == 1511, "Maj", "August")) |> 
  select(-party_e, -party_p, -noparty, -source, -year, -month, -day, -id, -pollingfirm)

farver <- c("Venstre" = "#459BC8", 
          "Socialdemokraterne" = "#E3515D", 
          "Dansk Folkeparti" = "#3D6F8D", 
          "SF" = "#9C1D2A", 
          "Veganerpartiet" = "green", 
          "Konservative" = "#429969", 
          "Radikale Venstre" = "#EB4295", 
          "Enhedslisten" = "#914A4F", 
          "Liberal Alliance" = "#EE9A5F", 
          "Nye Borgerlige" = "#05454F", 
          "Alternativet" = "#AEFEAF",
          "Kristendemokraterne" = "#F4CE97")

megafon |> 
  pivot_longer(party_a:party_aa) |> 
  pivot_wider(names_from = maaling, values_from = c(n, value)) |> 
  mutate(ci = 1.96 * sqrt( value_August * ( (100 - value_August) / n_August) + value_Maj * ( (100 - value_Maj) / n_Maj)  ) ) |> 
  mutate(forskel = value_August - value_Maj) |> 
  mutate(party_name = case_when(
    name == "party_v" ~ "Venstre",
    name == "party_a" ~ "Socialdemokraterne",
    name == "party_o" ~ "Dansk Folkeparti",
    name == "party_f" ~ "SF",
    name == "party_g" ~ "Veganerpartiet",
    name == "party_c" ~ "Konservative",
    name == "party_b" ~ "Radikale Venstre",
    name == "party_oe" ~ "Enhedslisten",
    name == "party_i" ~ "Liberal Alliance",
    name == "party_d" ~ "Nye Borgerlige",
    name == "party_aa" ~ "Alternativet",
    name == "party_k" ~ "Kristendemokraterne"
  )) |> 
  mutate(party_name = fct_reorder(party_name, forskel)) |> 
  ggplot(aes(party_name, forskel, ymin = forskel - ci, ymax = forskel + ci, colour = party_name)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 2) + 
  geom_errorbar(size = 1.1, width = 0) +
  theme_minimal() +
  coord_flip() +
  scale_colour_manual(values = farver) +
  labs(x = NULL,
       y = "Forskel i procentpoint (m. 95% konfidensinterval)") +
  theme(legend.position = "none")

ggsave("megafon_forskel.png", width = 7, height = 4)

