# R script for "How to improve your figures #13: Show the numbers"
# URL: https://erikgahner.dk/2024/how-to-improve-your-figures-13-show-the-numbers/

library("haven")
library("ggplot2")
library("dplyr")

# material is available here: https://doi.org/10.7910/DVN/U090WG
df <- read_dta("Heinzel_Weaver_Jorgensen_24_APSR_project_data.dta")

df |>
    group_by(overallscore) |>
    summarise(prop = n() / nrow(df)) |>
    ggplot(aes(overallscore, prop, label = paste0(round(prop * 100), "%"))) +
    geom_col(fill = "gray90", colour = "gray40") +
    geom_text(aes(y = prop - 0.03)) +
    scale_y_continuous(labels = scales::percent_format()) +
    theme_minimal() +
    labs(x = "Gender Mainstreaming Index",
         y = "Percent")

ggsave("heinzeletaAPSR_02.png", width = 4, height = 3, bg = "white")
