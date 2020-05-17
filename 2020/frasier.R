# R script to "Frasier"
# Link: 

library("tidyverse")
library("ggrepel")

theme_set(theme_minimal())

frasier_raw <- read_csv("ratings.csv")

frasier <- frasier_raw %>% 
  filter(str_detect(Title, "Frasier:")) %>% 
  mutate(Title = str_remove_all(Title, "Frasier: ")) %>% 
  select(title = Title, date = `Release Date`, me = `Your Rating`, imdb = `IMDb Rating`, numvotes = `Num Votes`, director = Directors)

frasier %>% 
  mutate(title_show = ifelse(me < 5 | me == 10, title, NA)) %>%
  ggplot(aes(date, me)) +
  geom_smooth(se = FALSE, colour = "#DDD1BA", size = 2) + 
  geom_point(alpha = .5) +
  theme_minimal() +
  labs(y = NULL,
       x = NULL) +
  scale_y_continuous(breaks = 3:10, labels = 3:10) +
  geom_label_repel(aes(label = title_show),
                   segment.color = 'grey50') 

ggsave("frasier_time.png", width = 10, height = 7)

frasier %>% 
  ggplot(aes(imdb, me)) +
  geom_jitter(shape = 1) +
  geom_smooth(method = "lm", se = FALSE, colour = "#DDD1BA", size = 2) + 
  labs(x = "IMDb rating",
       y = "My rating")

ggsave("frasier_lm.png", width = 10, height = 7)

# R2 = .27
frasier %>% 
  lm(me ~ imdb, data = .) %>% 
  summary()

# Adjusted R2 = .32
frasier %>% 
  lm(me ~ imdb + date + numvotes + director, data = .) %>% 
  summary()

