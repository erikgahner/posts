# R script for "Breaking Bad bliver bedre #2"
# URL: https://erikgahner.dk/2022/breaking-bad-bliver-bedre-2/

library("tidyverse")
library("gt")

imdb_raw <- read_csv("ratings.csv")

bb <- imdb_raw |> 
  filter(str_detect(`Title`, "Breaking Bad:")) |> 
  arrange(`Release Date`) |> 
  mutate(id = row_number(), .before = 1) |> 
  mutate(season = factor(case_when(
    between(id, 1, 7) ~ 1,
    between(id, 8, 20) ~ 2,
    between(id, 21, 33) ~ 3,
    between(id, 34, 46) ~ 4,
    TRUE ~ 5,
  ))) |> 
  mutate(title = str_remove(Title, "Breaking Bad: "))

bb |> 
  group_by(season) |> 
  summarise(afsnit = n(),
            rating = mean(`IMDb Rating`),
            rating_min = min(`IMDb Rating`),
            rating_max = max(`IMDb Rating`),
            rating_erik = mean(`Your Rating`),) |> 
  gt() |> 
  cols_label(season = "Sæson",
             afsnit = "Afsnit",
             rating = "IMDb gennemsnit",
             rating_min = "IMDb minimum",
             rating_max = "IMDb maximum",
             rating_erik = "Mit gennemsnit") |> 
  fmt_number(columns = c(rating, rating_erik), decimals = 1) |> 
  gtExtras::gt_theme_espn() |> 
  as_raw_html()
  
bb |> 
  ggplot(aes(id, `IMDb Rating`, group = season, colour = season)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  ggthemes::scale_color_gdocs() +
  scale_x_continuous(breaks = c(1, 8, 21, 34, 47, 62),
                     labels = c("S1", "S2", "S3", "S4", "S5", "")) +
  labs(colour = "Sæson",
       x = NULL,
       y = "IMDb gennemsnit") +
  theme(legend.position = "none")

ggsave("breakingbad_01.png", width = 6, height = 5, bg = "white")
  
bb |> 
  ggplot(aes(id, `IMDb Rating`, group = season, colour = season)) +
  geom_line(stat="smooth",method = "lm",
            size = 1.5,
            alpha = 0.2) +
  geom_point() +
  geom_text(aes(label = title), size = 3, hjust = 0, nudge_y = 0.03) +
  coord_flip() +
  theme_minimal() +
  labs(x = NULL,
       y = "IMDb gennemsnit") +
  theme(legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  ylim(7.9, 10.2) 

ggsave("breakingbad_02.png", width = 7, height = 8, bg = "white")

