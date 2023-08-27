# R script for "Twenty years of Marginal Revolution"
# URL: https://erikgahner.dk/2023/twenty-years-of-marginal-revolution/

library("tidyverse")
library("MRposts") # https://github.com/bldavies/MRposts

categories <- MRposts::categories |> as_tibble()
metadata <- MRposts::metadata |> as_tibble()

# Get info on first and final post in data
summary(metadata$time)

categories |> 
  count(category) |> 
  mutate(category = fct_reorder(category, n)) |> 
  ggplot(aes(category, n)) +
  geom_col() + 
  coord_flip() +
  theme_bw() +
  labs(x = NULL,
       y = NULL)

ggsave("margrev_categories.png", bg = "white", width = 4, height = 5)
