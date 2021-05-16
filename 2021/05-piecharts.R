# R script for "How to improve your figures #5: Don’t use pie charts"
# Link: https://erikgahner.dk/2021/how-to-improve-your-figures-5-dont-use-pie-charts/

library("tidyverse")

df <- data.frame(
  sector = c("Public-sector lawyers", "Private-sector lawyers", "Farm owners/managers", 
             "Business owners/execs", "Business employee", "Profit-oriented professional",
             "Service-based professional", "Workers/poor", "Military/law enforcement", 
             "Political office", "Other/unknown"),
  value = c(.05, .16, .02, .11, .09, .04, .10, .02, .06, .35, 0)
)

df %>% 
  mutate(sector = fct_reorder(sector, value)) %>% 
  ggplot(aes(sector, value)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = NULL,
       y = "Occupations of members of Congress") +
  theme_minimal()

ggsave("piechart_03.png", width = 5, height = 3)
