# R script for "Against nudging"
# URL: https://erikgahner.dk/2023/against-nudging/

library("tidyverse")

# Get data from https://osf.io/fywae/
df <- read_csv("mhhb_nma_data_corrected.csv") |> 
  mutate(negative = ifelse(cohens_d < 0 , "Yes", "No"))

summary(df$cohens_d)
table(df$negative)

df |> 
  ggplot(aes(cohens_d, fill = negative, group = negative)) +
  geom_histogram(bins = 30, colour = "lightgray") +
  ggthemes::scale_fill_fivethirtyeight() +
  labs(caption = "Source: Corrected data from Mertens et al. (2022)",
       x = "Effect size (Cohen's d) →",
       y = "Count →") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.y = element_text(hjust = 0),
    axis.title.x = element_text(hjust = 0),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )

ggsave("nudging_negativeeffects.png", width = 6, height = 4, bg = "white")
