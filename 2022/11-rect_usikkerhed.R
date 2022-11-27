# R script for "Statistisk usikkerhed med mønster i R"
# URL: https://erikgahner.dk/2022/statistisk-usikkerhed-med-monster-i-r/

library("tidyverse")
library("ggpattern")

polls <- read_csv("polls.csv",
                  col_types = cols(
                    party_p = col_double(),
                    party_e = col_double(),
                    party_g = col_double()
                  ))

poll_recent <- polls |> 
  slice(1647) |> 
  select(-party_e, -party_p, -noparty, -source, -year, -month, -day, -id, -pollingfirm) |> 
  pivot_longer(party_a:party_ae) |> 
  drop_na(value) |> 
  mutate(ci = 1.96 * sqrt(value * ((100 - value) / n))) |> 
  mutate(party_name = case_when(
    name == "party_v" ~ "Venstre",
    name == "party_a" ~ "Socialdemokraterne",
    name == "party_o" ~ "Dansk Folkeparti",
    name == "party_f" ~ "SF",
    name == "party_g" ~ "Veganerpartiet",
    name == "party_q" ~ "Frie Grønne",
    name == "party_ae" ~ "Danmarksdemokraterne",
    name == "party_c" ~ "Konservative",
    name == "party_m" ~ "Moderaterne",
    name == "party_b" ~ "Radikale Venstre",
    name == "party_oe" ~ "Enhedslisten",
    name == "party_i" ~ "Liberal Alliance",
    name == "party_d" ~ "Nye Borgerlige",
    name == "party_aa" ~ "Alternativet",
    name == "party_k" ~ "Kristendemokraterne"
  )) |> 
  filter(party_name != "Frie Grønne") |> 
  arrange(value) |> 
  mutate(party_name = fct_reorder(party_name, value)) |> 
  mutate(xstart2 = row_number() - 0.45, xend2 = row_number() + 0.45) |> 
  mutate(farver = case_when(
    party_name == "Venstre" ~ "#459BC8", 
    party_name == "Socialdemokraterne" ~ "#E3515D", 
    party_name == "Dansk Folkeparti" ~ "#3D6F8D", 
    party_name == "SF" ~ "#9C1D2A", 
    party_name == "Veganerpartiet" ~ "green", 
    party_name == "Konservative" ~ "#429969", 
    party_name == "Radikale Venstre" ~ "#EB4295", 
    party_name == "Enhedslisten" ~ "#914A4F", 
    party_name == "Liberal Alliance" ~ "#EE9A5F", 
    party_name == "Nye Borgerlige" ~ "#05454F", 
    party_name == "Alternativet" ~ "#AEFEAF",
    party_name == "Kristendemokraterne" ~ "#F4CE97",
    party_name == "Stram Kurs" ~ "#000000",
    party_name == "Frie Grønne" ~ "green",
    party_name == "Moderaterne" ~ "#7a2f8b",
    party_name == "Danmarksdemokraterne" ~ "#b23835"
  )) 

poll_recent |>
  mutate(value = value / 100,
         ci = ci / 100) |> 
  ggplot(aes(party_name, value, ymin = value - ci, ymax = value + ci, fill = party_name)) +
  geom_col() + 
  geom_rect_pattern(aes(xmin = xstart2, xmax = xend2, ymin = value - ci, ymax = value + ci),
                               fill = NA, pattern_fill = colorspace::lighten(poll_recent$farver, 0.3), 
                               pattern_color = NA, pattern_spacing = 0.0075, pattern_density = 0.4) +
  theme_minimal() +
  coord_flip() +
  scale_fill_manual(values = poll_recent$farver) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y = NULL,
       x = NULL) +
  theme(panel.grid.major.y = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        legend.title = element_text(face = "bold"),
        legend.position = "none")

ggsave("rect_usikkerhed.png", width = 6, height = 3, bg = "white")
