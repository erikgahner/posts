# R script for "The reliability of flight emission calculators"
# Link: https://erikgahner.dk/2021/the-reliability-of-flight-emission-calculators/

library("tidyverse")
library("quantmod")

prices_raw <- read_csv("10-carboncalculator.csv")

exchange_rates <- data.frame(getQuote(paste0(c("EUR", "USD", "CAD", "ZAR"), c("GBP"), "=X"))) %>% 
  mutate(exchange = rownames(.))

prices <- prices_raw %>% 
  mutate(price_converted = case_when(
    price_currency == "GBP" ~ price_amount,
    price_currency == "EUR" ~ exchange_rates$Last[exchange_rates$exchange == "EURGBP=X"] * price_amount,
    price_currency == "USD" ~ exchange_rates$Last[exchange_rates$exchange == "USDGBP=X"] * price_amount,
    price_currency == "CDN" ~ exchange_rates$Last[exchange_rates$exchange == "CADGBP=X"] * price_amount,
    price_currency == "ZAR" ~ exchange_rates$Last[exchange_rates$exchange == "ZARGBP=X"] * price_amount,
    price_currency == "CAD" ~ exchange_rates$Last[exchange_rates$exchange == "CADGBP=X"] * price_amount,
    TRUE ~ NA_real_
  )) %>% 
  mutate(trip = case_when(
    trip_via == "N/A" & trip_destination == "JFK" ~ "LHR to JFK",
    trip_via == "AMS" & trip_destination == "JFK" ~ "LHR to JFK via AMS",
    trip_via == "N/A" & trip_destination == "AMS" ~ "LHR to AMS"
  ))

prices %>% 
  filter(trip_via == "N/A", trip_destination == "JFK", class == "First")

prices %>% 
  filter(class != "N/A", kg < 6000) %>% 
  rename(Trip = trip, Class = class) %>% 
  ggplot(aes(kg, price_converted, group = company, colour = Class, shape = Class)) +
  geom_smooth(se = FALSE, method = "lm", colour = "lightgray", size = 0.5) +
  geom_point() +
  theme_minimal() +
  ggthemes::scale_color_gdocs() +
  theme(legend.position = "bottom") +
  labs(y = "Price (GBP)",
       x = "kg CO2",
       colour = NULL,
       shape = NULL) +
  facet_grid(~ Trip, scales = "free")

ggsave("co2prices.png", width = 7, height = 4)
