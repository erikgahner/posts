# R script for "How to improve your figures #11: Do not repeat information"
# URL: https://erikgahner.dk/2022/how-to-improve-your-figures-11-do-not-repeat-information/

library("tidyverse")

df <- read_csv("big_five.csv")

df |>
  pivot_longer(cols = 3:7, names_to = "Trait") |>
  ggplot(aes(x = fct_reorder(Country, Group), y = value, shape = Group)) +
  geom_point(size = 3) +
  facet_grid(vars(Trait), switch = "y") +
  labs(title = "Omega (ω) Correlation Coefficients for the BF-10 Traits Across 28 Countries",
       shape = NULL,
       x = NULL,
       y = NULL,
       caption = str_wrap("Within-trait inter-item correlations coefficients (Omega) for the BF-10 by country, grouped into Western (▲) and non-Western (●) countries. Adapted from Ludeke and Larsen (2017).", width = 95)) +
  theme_bw() +
  theme(legend.position="none") + 
  scale_fill_manual(values = c("#000000", "#999999")) +
  scale_y_continuous(limits = c(0, 1), position = "right") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) 

ggsave("repeatinformation_02.png", width = 10, height = 8)

df |>
  pivot_longer(cols = 3:7, names_to = "Trait") |>
  ggplot(aes(x = fct_reorder(Country, Group), y = value, shape = Group)) +
  geom_point(size = 3) +
  facet_wrap(~ Trait, ncol = 5) +
  labs(title = "Omega (ω) Correlation Coefficients for the BF-10 Traits Across 28 Countries",
       shape = NULL,
       x = NULL,
       y = NULL,
       caption = str_wrap("Within-trait inter-item correlations coefficients (Omega) for the BF-10 by country, grouped into Western (▲) and non-Western (●) countries. Adapted from Ludeke and Larsen (2017).", width = 95)) +
  theme_bw() +
  theme(legend.position="none") + 
  scale_fill_manual(values = c("#000000", "#999999")) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25), labels = c("0", "0.25", "0.5", "0.75", "1")) +
  coord_flip()

ggsave("repeatinformation_03.png", width = 10, height = 8)
