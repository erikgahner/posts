# R script to "The Sopranos"
# Link: https://erikgahner.dk/2019/the-sopranos/

library("tidyverse")
library("ggrepel")

sopranos <- read_csv("ratings.csv")

sopranos <- sopranos %>% 
  filter(str_detect(Title, "The Sopranos:")) %>% 
  mutate(Title = str_remove(Title, "The Sopranos: ")) %>% 
  select(title = Title, date = `Release Date`, me = `Your Rating`, imdb = `IMDb Rating`, director = Directors)

mean(sopranos$me)
mean(sopranos$imdb)
cor(sopranos$me, sopranos$imdb)

sopranos %>% 
  mutate(title_show = ifelse(me < 6 | me == 10, title, NA)) %>%
  ggplot(aes(date, me)) +
  geom_point(shape = 1) +
  geom_smooth(se = FALSE, colour = "#a52a2a") + 
  theme_minimal() +
  labs(y = NULL,
       x = NULL) +
  scale_y_continuous(breaks = 3:10, labels = 3:10) +
  geom_label_repel(aes(label = title_show),
                   box.padding   = 0.6, 
                   point.padding = 0.2,
                   segment.color = 'grey50') 
  
ggsave("sopranos.png", width = 7, height = 7)
