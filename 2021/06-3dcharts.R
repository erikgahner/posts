# R script for "How to improve your figures #7: Don’t use a third dimension"
# Link: https://erikgahner.dk/2021/how-to-improve-your-figures-7-dont-use-a-third-dimension/

library("tidyverse")

df <- data.frame(
  value = c("Sec", "Con", "Tra", "Ben", "Uni", "SDir", "Sti", "Hed", "Ach", "Pow"),
  correlation = c(-.2, -.1, -.07, .18, .28, .08, .03, -.01, -.08, -.14)) %>% 
  mutate(value = fct_relevel(value, c("Sec", "Con", "Tra", "Ben", "Uni", "SDir", "Sti", "Hed", "Ach", "Pow")))

df %>% 
  ggplot(aes(value, correlation)) +
  geom_col() +
  labs(x = "Value",
       y = "Correlation") +
  theme_minimal()

ggsave("3d_03.png", width = 4, height = 3)


