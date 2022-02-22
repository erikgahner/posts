# R script for "Frasier #2"
# URL: https://erikgahner.dk/2022/frasier-2/

library("tidyverse")
library("gt")

imdb_knud <- read_csv("ratings_knud.csv") |> 
  filter(str_detect(Title, "Frasier: "), `Title Type` == "tvEpisode") |> 
  mutate(rating_knud = `Your Rating`)
  
imdb_erik <- read_csv("ratings_erik.csv") |> 
  filter(str_detect(Title, "Frasier: "), `Title Type` == "tvEpisode") |> 
  mutate(rating_erik = `Your Rating`)

# 110 hours of content
sum(imdb_erik$`Runtime (mins)`) / 60 

# Make sure we have rated the same episodes
imdb_erik |> filter(!Title %in% unique(imdb_knud$Title))
imdb_knud |> filter(!Title %in% unique(imdb_erik$Title))

imdb <- imdb_erik |> 
  select(title = Title, date = `Release Date`, rating_imdb = `IMDb Rating`, rating_erik) |> 
  left_join(select(imdb_knud, title = Title, rating_knud), by = "title") |> 
  mutate(title = str_replace(title, "Frasier: ", ""))

imdb |> 
  pivot_longer(rating_imdb:rating_knud, names_to = "rater", values_to = "rating") |> 
  mutate(rater = case_when(
    rater == "rating_imdb" ~ "IMDb",
    rater == "rating_knud" ~ "Knud",
    rater == "rating_erik" ~ "Erik"
  )) |> 
  ggplot(aes(date, rating, group = rater, colour = rater)) +
  geom_jitter(alpha = .7) +
  geom_smooth(se = FALSE) +
  scale_y_continuous(limits = c(1,10), breaks = 1:10, labels = 1:10) +
  theme_minimal() +
  ggthemes::scale_color_gdocs() +
  labs(x = NULL,
       y = "Rating",
       colour = NULL) +
  theme(legend.position = "bottom")

ggsave("frasier-all.png", width = 6, height = 5)

summary(imdb$rating_erik)
summary(imdb$rating_knud)

imdb |> 
  select(rating_knud, rating_erik, rating_imdb) |> 
  cor(use = "pairwise.complete.obs")

imdb |> 
  filter(rating_knud >= 9, rating_erik >= 9) |> 
  arrange(date) |> 
  gt() |> 
  gtExtras::gt_theme_espn()


imdb |> 
  filter(date < as.Date("1994-05-20")) |> 
  summarise(rating_imdb = mean(rating_imdb),
            rating_erik = mean(rating_erik),
            rating_knud = mean(rating_knud))

imdb |> 
  mutate(dif = abs(rating_knud - rating_erik)) |> 
  filter(dif < 2) |> 
  NROW()

imdb |> 
  mutate(dif = abs(rating_knud - rating_erik)) |> 
  filter(dif > 3) |> 
  NROW()

imdb |> 
  mutate(dif = abs(rating_knud - rating_erik)) |> 
  arrange(desc(dif)) |> 
  filter(dif >= 4) |>
  select(-date, -rating_imdb) |> 
  gt() |> 
  gtExtras::gt_theme_espn() |> 
  cols_label(title = "Title",
             rating_erik = "Rating: Erik",
             rating_knud = "Rating: Knud",
             dif = "Difference") |> 
  as_raw_html()

imdb |> 
  ggplot(aes(rating_erik, rating_knud, label = title)) +
  geom_smooth(method = "lm", se = FALSE)+
  geom_jitter(width = 0.3, height = 0.3, alpha = 0.5) +
  theme_minimal() +
  scale_x_continuous(limits = c(1,10), breaks = 1:10, labels = 1:10) +
  scale_y_continuous(limits = c(1,10), breaks = 1:10, labels = 1:10) +
  labs(y = "Rating: Knud",
       x = "Rating: Erik") 
  
ggsave("frasier-episodes.png", width = 6, height = 5)

imdb |> write_csv("frasier_ratings.csv")
