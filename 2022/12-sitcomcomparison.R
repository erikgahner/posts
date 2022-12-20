# R script for "Frasier, Seinfeld and Friends"
# URL: https://erikgahner.dk/2022/frasier-seinfeld-and-friends/

library("tidyverse")
library("lubridate")
library("gt")

ratings_raw <- read_csv("ratings.csv")

ratings <- ratings_raw |> 
  filter(Year < 2005) |> 
  filter(str_detect(`Title`, "Frasier:|Seinfeld:|Friends:")) |> 
  select(title = Title, rating = `IMDb Rating`, date = `Release Date`) |> 
  mutate(tv = case_when(
    str_detect(title, "Frasier:") ~ "Frasier",
    str_detect(title, "Friends:") ~ "Friends",
    str_detect(title, "Seinfeld:") ~ "Seinfeld"
    )) |> 
  mutate(yearweek = paste0(year(date), "-", week(date)))

ratings_seinfeld <- ratings |> 
  filter(tv == "Seinfeld") |> 
  arrange(date) |>
  mutate(id = row_number()) |> 
  mutate(season = case_when(
    id %in% 1:5 ~ 1,
    id %in% 6:17 ~ 2,
    id %in% 18:39 ~ 3,
    id %in% 40:62 ~ 4,
    id %in% 63:83 ~ 5,
    id %in% 84:106 ~ 6,
    id %in% 107:128 ~ 7,
    id %in% 129:150 ~ 8,
    id %in% 151:172 ~ 9
  )) |> 
  group_by(season) |>  
  mutate(episode = row_number())

ratings |> 
  group_by(tv) |> 
  summarise(rating = mean(rating))

seinfeld_table <- ratings_seinfeld |> 
  select(season, episode, rating) |> 
  mutate(season = paste0("S", str_pad(season, 2, "left", "0"))) |> 
  pivot_wider(names_from = season, values_from = rating) |> 
  mutate(episode = paste0("E", str_pad(episode, 2, "left", "0"))) 


seinfeld_table_gt <- seinfeld_table |> 
  ungroup() |> 
  gt() |> 
  cols_label(
    episode = ""
  ) |>
  sub_missing(missing_text = "") |>
  data_color(
    columns = 2:last_col(),
    colors = scales::col_numeric(
      palette = c("red", "yellow", "darkgreen"),
      na.color = "white",
      domain = c(5.5, 10)
    )
  ) |> 
  tab_style(
    cell_text(
      # font = "IBM Plex Mono",
      align = "right"
    ),
    locations = list(
      cells_body(columns = 2:last_col())
    )
  ) |> 
  tab_style(
    cell_text(
      weight = "bold"
    ),
    locations = list(
      cells_title(groups = c("title"))
    )
  ) |>
  tab_style(
    list(
      cell_borders(sides = "all", color = "white", style = "solid", weight = px(1.5))),
    locations = list(
      cells_body()
    )
  ) |> 
  tab_style(
    cell_text(),
    locations = list(
      cells_column_labels(gt::everything()),
      cells_body(columns = 1)
    )
  ) |> 
  tab_options(
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    column_labels.border.top.style = "hidden",
    column_labels.border.bottom.style ="hidden"
  ) 

seinfeld_table_gt

as_raw_html(seinfeld_table_gt)


