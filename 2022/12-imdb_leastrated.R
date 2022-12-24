# R script for "Lesser known movies I like"
# URL: https://erikgahner.dk/2022/lesser-known-movies-i-like/

library("tidyverse")
library("gt")

imdb_raw <- read_csv("ratings.csv")

imdb <- imdb_raw |> 
  filter(`Title Type` == "movie", 
         `Num Votes` < as.numeric(quantile(imdb_raw$`Num Votes`[imdb_raw$`Title Type` == "movie"], 0.25)),
         `Your Rating` >= 8,
         `IMDb Rating` < 8,
         !str_detect(Genres, "Documentary")) |> 
  arrange(desc(`Your Rating`), `Num Votes`) |> 
  group_by(Year) |> 
  filter(row_number() == 1) |> 
  ungroup()


imdb |> 
  arrange(desc(Year)) |> 
  mutate(Title_URL = sprintf('<p align = "left">%s (<a href = "%s">%s</a>)</p>', Title, URL, Year),
         Title_URL = map(Title_URL, gt::html)) |> 
  select(Title_URL, Genres, `Num Votes`, `IMDb Rating`) |> 
  gt() |> 
  data_color(
    columns = `IMDb Rating`,
    colors = scales::col_numeric(
      palette = c("#FF4136", "#FFDC00", "#2ECC40"),
      na.color = "white",
      domain = c(min(imdb$`IMDb Rating`), max(imdb$`IMDb Rating`))
    )
  ) |> 
  fmt_number(`Num Votes`, decimals = 0) |> 
  cols_label(
    Title_URL = "Movie",
    `Num Votes` = "# votes"
  ) |> 
  as_raw_html()

