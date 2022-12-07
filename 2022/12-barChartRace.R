# R script for "Opbakningen til de fire gamle partier, 1953-2022"
# URL: https://erikgahner.dk/2022/opbakningen-til-de-fire-gamle-partier-1953-2022/

library("tidyverse")
library("ddplot")

votes_raw <- read_csv("https://raw.githubusercontent.com/Straubinger/folketingsvalg/master/votes.csv")

votes <- votes_raw |> 
  pivot_longer(starts_with("party_")) |> 
  select(year, name, value, total_valid) |> 
  mutate(navn = case_when(
    name == "party_s" ~ "Socialdemokratiet",
    name == "party_rv" ~ "Det Radikale Venstre",
    name == "party_k" ~ "Det Konservative Folkeparti",
    name == "party_cd" ~ "Centrum-Demokraterne",
    name == "party_rfb" ~ "Retsforbundet",
    name == "party_sf" ~ "Socialistisk Folkeparti",
    name == "party_dkp" ~ "Danmarks Kommunistiske Parti",
    name == "party_df" ~ "Dansk Folkparti",
    name == "party_fk" ~ "Fælles Kurs",
    name == "party_lc" ~ "Liberalt Centrum",
    name == "party_kd" ~ "Kristendemokraterne",
    name == "party_sp" ~ "Slesvigsk Parti",
    name == "party_u" ~ "De Uafhængige",
    name == "party_v" ~ "Venstre",
    name == "party_vs" ~ "Venstresocialisterne",
    name == "party_fp" ~ "Fremskridtspartiet",
    name == "party_el" ~ "Enhedslisten",
    name == "party_la" ~ "Liberal Alliance",
    name == "party_alt" ~ "Alternativet",
    name == "party_nb" ~ "Nye Borgerlige",
    name == "party_krp" ~ "Klaus Riskær Pedersen",
    name == "party_sk" ~ "Stram Kurs",
    name == "party_m" ~ "Moderaterne",
    name == "party_fg" ~ "Frie Grønne",
    name == "party_dd" ~ "Danmarksdemokraterne",
    TRUE ~ name
  )) |> 
  drop_na(value) |> 
  mutate(procent = (value / total_valid) * 100)

barChartRace(
  data = filter(votes, navn %in% c("Socialdemokratiet", "Venstre", "Det Konservative Folkeparti", "Det Radikale Venstre")),
  bgcol = "white",
  panelcol = "#f6f6f6",
  frameDur = 1000,
  colorCategory = c("Set1"),
  x = "procent",
  y = "navn",
  time = "year",
  xtitle = "Opbakning (%)") |> 
  htmlwidgets::saveWidget("barChartRace.htm", selfcontained = FALSE)
