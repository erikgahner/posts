# R script for "Tabel med valgresultatet i R"
# URL: https://erikgahner.dk/2022/tabel-med-valgresultatet-i-r/

library("tidyverse")
library("gt")

votes_raw <- read_csv("https://raw.githubusercontent.com/Straubinger/folketingsvalg/master/votes.csv")
seats_raw <- read_csv("https://raw.githubusercontent.com/Straubinger/folketingsvalg/master/seats.csv")

votes <- votes_raw |> 
  pivot_longer(starts_with("party_")) |> 
  select(year, name, value, total_valid)

seats <- seats_raw |> 
  pivot_longer(starts_with("party_")) |> 
  select(year, name, seats = value)

full <- left_join(votes, seats, by = c("name", "year")) |> 
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
  ))


tabel_resultater <- full |> 
  filter(year == 2022) |> 
  drop_na(value) |> 
  mutate(value_rel = value / total_valid) |> 
  select(-year) |> 
  select(navn, value_rel, seats, value) |> 
  arrange(desc(value_rel)) |> 
  gt() |> 
  fmt_percent(columns = value_rel, decimals = 1,
              sep_mark = ".", dec_mark = ",") |> 
  fmt_number(columns = c("value", "seats"), decimals = 0, sep_mark = ".", dec_mark = ",") |> 
  tab_style(
    style = cell_fill(color = "#F3E8EA"),
    locations = cells_body(
      rows = value_rel < 0.02)
  ) |> 
  text_transform(
    locations = cells_body(columns = c(navn)), 
    fn = function(x){ glue::glue("<div><span style='font-weight:bold;font-variant:small-caps;font-size:14px'>{x}</div>") }) |> 
  tab_style(
    style = cell_text(
      size = px(12),
      color = "#999",
      indent = px(40)
    ),
    locations = cells_body(columns = value)
  ) |> 
  tab_style(
    style = cell_text(
      size = px(11),
      color = "#999",
      transform = "uppercase"
    ),
    locations = cells_column_labels(everything())
  ) |> 
  tab_options(
    column_labels.border.top.style = "none",
    table.border.top.style = "none",
    column_labels.border.bottom.style = "none",
    column_labels.border.bottom.width = 1,
    column_labels.border.bottom.color = "#334422",
    table_body.border.top.style = "none",
    table_body.border.bottom.color = "#0000001A",
    data_row.padding = px(7)
  ) |> 
  cols_label(navn = "Parti",
             value_rel = "Stemmer (%)",
             seats = "Mandater",
             value = "Antal stemmer") 


tabel_deltagelse <- votes_raw |> 
  select(year, blank, other_invalid, total_votes, electorate) |> 
  filter(year == 2022) |> 
  mutate(turnout = total_votes / electorate) |> 
  select(turnout, blank, other_invalid, total_votes, electorate) |> 
  gt() |> 
  cols_label(turnout = "Stemmeprocent",
             blank = "Blanke stemmer",
             other_invalid = "Andre ugyldige stemmer",
             total_votes = "Antal stemmer",
             electorate = "Stemmeberettigede") |> 
  fmt_percent(columns = turnout, decimals = 1,
              sep_mark = ".", dec_mark = ",") |>
  tab_options(
    column_labels.border.top.style = "none",
    table.border.top.style = "none",
    column_labels.border.bottom.style = "none",
    column_labels.border.bottom.width = 1,
    column_labels.border.bottom.color = "#334422",
    table_body.border.top.style = "none",
    table_body.border.bottom.color = "#0000001A",
    data_row.padding = px(7)
  ) |> 
  fmt_number(columns = c("blank", "other_invalid", "total_votes", "electorate"), decimals = 0, sep_mark = ".", dec_mark = ",") |> 
  tab_style(
    style = cell_text(
      size = px(11),
      color = "#999",
      transform = "uppercase"
    ),
    locations = cells_column_labels(everything())
  ) 

glue::glue("<h4>Opbakning til de politiske partier ved folketingsvalget i 2022</h4>", 
           as_raw_html(tabel_resultater), "<br>",
           "<h4>Valgdeltagelsen ved folketingsvalget i 2022</h4>",
           as_raw_html(tabel_deltagelse), "<br>",
           "<b>Kilde:</b> <a href='https://github.com/Straubinger/folketingsvalg'>https://github.com/Straubinger/folketingsvalg</a>")

