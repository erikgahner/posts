# R script for "Årets første sommerdag"
# URL: https://erikgahner.dk/2022/arets-forste-sommerdag/

library("tidyverse")
library("lubridate")

Sys.setlocale("LC_TIME", "da_DK.UTF-8")

df <- data.frame(dato = as.Date(c("2005-05-26", "2006-05-10", "2007-04-26",
                                  "2008-05-09", "2009-05-26", "2010-06-28",
                                  "2011-05-09", "2012-05-21", "2013-05-17",
                                  "2014-05-21", "2015-06-13", "2016-05-08",
                                  "2017-05-18", "2018-04-19", "2019-06-02",
                                  "2020-06-01", "2021-05-10", "2022-05-18"))) |> 
  mutate(year = year(dato),
         year_day = yday(dato)) |> 
  mutate(dato_tekst = gsub(" 0", " ", tolower(format(dato, " %d. %B")))) 


df |> 
  ggplot(aes(x = year, y = year_day)) +
  geom_col(fill = "#FFDC00") +
  theme_minimal() +
  coord_flip() +
  geom_text(aes(label = dato_tekst), hjust = 1.1) +
  geom_text(aes(label = year, y = 5)) +
  labs(title = "Årets første sommerdag (>25 °C)",
       y = "Dage siden 1. januar",
       x = NULL) +
  theme(panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

ggsave("dmi.png", width = 8, height = 5, bg = "white")

