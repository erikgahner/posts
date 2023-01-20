# R script for "Tyler Cowen’s favourite things"
# URL: https://erikgahner.dk/2023/tyler-cowens-favourite-things/

library("tidyverse")
library("ggmap")
library("reactable")

things_raw <- read_csv("cowen_things.csv")
places_raw <- read_csv("cowen_places.csv")

things_raw |> 
  count(category) |> 
  mutate(category = fct_reorder(str_to_title(category), -n))  |> 
  ggplot(aes(category, n, fill = category)) +
  geom_col() +
  theme_minimal() +
  ggthemes::scale_fill_gdocs() +
  theme(legend.position = "none") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(x = NULL,
       y = NULL)

ggsave("cowen_categories.png", width = 6, height = 3, bg = "white")

things_movies <- things_raw |> 
  drop_na(imdb_ratings)

things_movies |> 
  mutate(imdb_label = case_when(
    imdb_average > 8.5 ~ imdb_title,
    imdb_average < 6.5 ~ imdb_title,
    imdb_ratings < 5000 ~ imdb_title,
    imdb_average > 8 & imdb_ratings < 50000 ~ imdb_title,
    TRUE ~ NA_character_
  )) |> 
  ggplot(aes(imdb_ratings, imdb_average, label = imdb_label)) +
  ggrepel::geom_label_repel() +
  geom_point(alpha = 0.5) +
  scale_x_log10(labels = scales::label_comma()) +
  theme_minimal() +
  labs(x = "Number of IMDb ratings",
       y = "Average IMDb rating")

ggsave("cowen_imdb.png", width = 8, height = 8, bg = "white")


worldmap <- borders("world", colour = "gray60", fill = "gray90", size = 0.05)

places_raw |> 
  drop_na(geo_lon, geo_lat) |> 
  ggplot(aes(geo_lon, geo_lat)) +
  worldmap +
  coord_cartesian(xlim = c(-165, 175), ylim = c(-50, 105)) +
  geom_point(aes(colour = category), alpha = 0.6) +
  theme_void() +
  theme(legend.position = "none") 

ggsave("cowen_map.png", width = 6, height = 3.5, bg = "white")

things_raw |> 
  left_join(select(places_raw, place, entity = category), by = "place") |> 
  mutate(entity = case_when(
    entity == "state" ~ "U.S. State",
    entity == "country" ~ "Country",
    entity == "city" ~ "City",
    entity == "region" ~ "Region",
    entity == "island" ~ "Island",
    entity == "planet" ~ "Planet"
  )) |> 
  mutate(category = str_to_title(category)) |> 
  select(-starts_with("imdb_")) |> 
  arrange(place, category) |> 
  select(-category) |> 
  rename(Entity = entity, Place = place, Category = subcategory, Thing = thing) |> 
  reactable(groupBy = c("Entity", "Place"),
            columns = list(
              Entity = colDef(minWidth = 120),
              Place = colDef(minWidth = 120),
              Thing = colDef(minWidth = 150)
            ), 
            style = list(fontFamily = "Work Sans, sans-serif", fontSize = "12px"),
            showPageInfo = FALSE) |> 
  htmlwidgets::saveWidget("cowen_table.html", selfcontained = FALSE)

