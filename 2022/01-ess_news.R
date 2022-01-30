# R script for "Book Review: Stop Reading the News"
# URL: https://erikgahner.dk/2022/book-review-stop-reading-the-news/

library("tidyverse")
library("haven")

# https://www.europeansocialsurvey.org/download.html?file=ESS9e03_1&y=2018
ess <- read_sav("ESS9e03_1.sav")

summary(ess$nwspol)

ess |> 
  filter(nwspol < quantile(nwspol, c(.99), na.rm = TRUE)) |> 
  ggplot(aes(nwspol)) +
  geom_histogram(fill = "lightgray", colour = "black", bins = 30) +
  theme_minimal() +
  geom_vline(xintercept = 30, colour = "#0074D9", linetype = "dashed") +
  geom_vline(xintercept = 60, colour = "#FF851B", linetype = "dotted") +
  geom_vline(xintercept = 90, colour = "#FF4136") +
  labs(x = "News consumption on a typical day, politics and current affairs (minutes)",
       y = NULL) 

ggsave("ess_nwspol.png", width = 6, height = 3)
