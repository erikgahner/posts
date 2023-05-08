# R script for "The many causes of Brexit #2"
# URL: https://erikgahner.dk/2023/the-many-causes-of-brexit-2/

library("tidyverse")
library("haven")

# https://osf.io/9y4sn/
df <- read_dta("Replication File for Political Behavior/Data Files/brexit.dta")

df |> 
  ggplot(aes(wind, remain, colour = region, label = city)) +
  geom_point(shape = 21, fill = "gray") +
  ggrepel::geom_label_repel() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Windspeed (in m/s)",
       y = "Remain (%)")

ggsave("remain_wind.png", bg = "white", width = 8, height = 8)

df |> 
  group_by(region) |> 
  summarise(wind = mean(wind),
            n = n()) |> 
  arrange(desc(wind))

df |> 
  lm(remain ~ wind, data = _) |> 
  summary()

df |> 
  lm(remain ~ wind + region, data = _) |> 
  summary()

df |> 
  mutate(engl = ifelse(region %in% c("Scotland", "Wales", "Northern Ireland"), 0, 1)) |> 
  lm(remain ~ wind + engl, data = _) |> 
  summary()

