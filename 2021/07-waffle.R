# R script for "How to improve your figures #8: Make probabilities tangible"
# Link: https://erikgahner.dk/2021/how-to-improve-your-figures-8-make-probabilities-tangible/

library("tidyverse")
library("waffle")

# Make sure to install FontAwesome fonts (the ttf files): https://github.com/FortAwesome/Font-Awesome/tree/master/webfonts

df <- data.frame(
  party = c("Democrats", "Republicans"),
  vals = c(75, 25)
  )

df %>%
  count(party, wt = vals) %>%
  ggplot(aes(label = party, values = n)) +
  geom_pictogram(n_rows = 10, aes(colour = party), flip = TRUE, make_proportional = TRUE) +
  scale_color_manual(
    name = NULL,
    values = c("#0074D9", "#FF4136"),
    labels = c("Democrats", "Republicans")
  ) +
  scale_label_pictogram(
    name = NULL,
    values = c("democrat", "republican"),
    labels = c("Democrats", "Republicans")
  ) +
  coord_equal() +
  theme_void() +
  theme(legend.position = "bottom") +
  theme_enhance_waffle() 

ggsave("waffleforecast.png", width = 6, height = 6)
