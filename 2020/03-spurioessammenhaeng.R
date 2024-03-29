# R script to "Spuriøs sammenhæng"
# Link: https://erikgahner.dk/2020/spurios-sammenhaeng/

library("dagitty")
library("ggdag")

dagitty("dag{{Y X} <- Z}") %>% 
  ggdag(layout = "circle") + 
  theme_dag()

ggsave("spurioes.png", width = 6, height = 4)