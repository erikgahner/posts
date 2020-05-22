# R script to figures in "Regeringens overblik over COVID-19 tests"
# Link: https://erikgahner.dk/2020/regeringens-overblik-over-covid-19-tests/

library("tidyverse")
library("rvest")
library("purrr")
library("ggthemes")

covid_raw <- read_html("https://www.worldometers.info/coronavirus/")

covid_tables <- html_nodes(covid_raw, "table")

covid_results <- covid_tables %>% 
  html_table(fill=TRUE) %>% 
    purrr::pluck(1)

covid <- covid_results %>% 
  rename(country = `Country,Other`, tests = `Tests/1M pop`) %>% 
  filter(nchar(tests) != 0) %>% 
  select(country, tests) %>% 
  mutate(test_n = parse_number(tests))

covid %>% 
  arrange(-test_n) %>% 
  top_n(20) %>% 
  mutate(country = fct_reorder(country, test_n),
         denmark = if_else(country == "Denmark", "Yes", "No")) %>% 
  ggplot(aes(x = country, y = test_n, fill = denmark)) +
  geom_col() + 
  coord_flip() +
  theme_bw() +
  ggthemes::scale_fill_fivethirtyeight() +
  theme(legend.position = "none") +
  labs(x = NULL,
       y = "Tests per mio. indbygger")

ggsave("covid19-tests.png", width = 8, height = 5)

owid_raw <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

names(owid)
max(owid_raw$date)

owid <- owid_raw %>% 
  drop_na(total_tests_per_thousand) %>% 
  filter(date == max(date)) %>% 
  select(country = location, tests = total_tests_per_thousand) 

owid %>% 
  arrange(-tests) %>% 
  top_n(20) %>% 
  mutate(country = fct_reorder(country, tests),
         denmark = if_else(country == "Denmark", "Yes", "No")) %>% 
  ggplot(aes(x = country, y = tests, fill = denmark)) +
  geom_col() + 
  coord_flip() +
  theme_bw() +
  ggthemes::scale_fill_fivethirtyeight() +
  theme(legend.position = "none") +
  labs(x = NULL,
       y = "Tests per tusinde indbygger")

ggsave("covid19-tests-owid.png", width = 8, height = 5)