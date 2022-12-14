# R script for "A dataset with political datasets #2"
# URL: https://erikgahner.dk/2022/a-dataset-with-political-datasets-2/

library("tidyverse")

poldata <- read_csv("https://raw.githubusercontent.com/erikgahner/PolData/master/PolData.csv")

poldata |> 
  count(category) |> 
  mutate(category = str_replace(str_to_title(category), " And ", " and ")) |> 
  mutate(category = fct_reorder(category, n)) |> 
  ggplot(aes(category, n)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL,
       y = NULL) +
  theme_minimal()

ggsave("poldata_categories.png", width = 5, height = 4, bg = "white")
