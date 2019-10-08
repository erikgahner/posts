# R script to "A Guide to Getting International Statistics into R"
# Link: https://erikgahner.dk/2019/a-guide-to-getting-international-statistics-into-r/

# load relevant packages
## data management etc.
library("tidyverse")
## the five packages to access data
library("WDI")
library("Rilostat")
library("OECD")
library("WHO")
library("eurostat")

# finding data
## search string
searchText <- "unemployment"

## World Bank
searchText %>%
  WDIsearch() %>%
  View()

## ILOSTAT
ilostat_list <- get_ilostat_toc()
ilostat_list %>%
  filter(str_detect(tolower(indicator.label), tolower(searchText))) %>%
  View()

## OECD
oecd_list <- get_datasets()
search_dataset(searchText, data = oecd_list) %>% 
  View()

## WHO
who_list <- get_codes()
who_list %>%
  filter(str_detect(tolower(display), tolower(searchText))) %>%
  View()

## Eurostat
eurostat_list <- get_eurostat_toc()
eurostat_list %>%
  filter(str_detect(tolower(title), tolower(searchText))) %>%
  View()

# get data
## World Bank
data_worldbank <- WDI(indicator = "SL.UEM.TOTL.ZS")

## ILOSTAT
data_ilostat <- get_ilostat(id = "UNE_DYAP_NOC_RT_A")

## OECD
data_oecd <- get_dataset(dataset = "AVD_DUR")

## WHO
data_who <- get_data("tfr")

## Eurostat
data_eurostat <- get_eurostat("ei_lmhr_m")

# create figure
data_worldbank %>% 
  drop_na(SL.UEM.TOTL.ZS) %>%
  filter(country %in% c("Denmark", "Norway", "Sweden")) %>%
  ggplot(aes(x = year, y = SL.UEM.TOTL.ZS, colour = country)) +
  geom_line(size = 1) +
  theme_minimal() + 
  labs(title = "Unemployment (% of total labor force), Scandinavia",
       colour = NULL,
       y = NULL,
       x = NULL) +
  theme(legend.position = "bottom")

ggsave("wdi-unemployment.png", width = 6, height = 4)

# Example: BIS
library("BIS")

## view available variables (only 22 observations; no need for a search)
bis_list <- BIS::get_datasets() 
bis_list %>%
  View()

## specify the name of the dataset (available in the 'name' column)
bis_name <- "Consumer prices"

## download data
data_bis <- bis_list %>%
  filter(name == bis_name) %>%
  select(url) %>%
  as.character() %>%
  get_bis()

