# R script for "My favourite links"
# URL: https://erikgahner.dk/2022/my-favourite-links/

library("tidyverse")

df_links <- read_csv("https://raw.githubusercontent.com/erikgahner/links/main/data.csv")

df_links |> 
  mutate(category = ifelse(nchar(category) > 2, str_to_title(category), category)) |> 
  mutate(category = ifelse(category == "tv", "TV", category)) |> 
  count(category) |> 
  filter(n > 1) |> 
  mutate(category = fct_reorder(category, n)) |> 
  ggplot(aes(category, n)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(y = NULL,
       x = NULL)

ggsave("links.png", width = 6, height = 6, bg = "white")
