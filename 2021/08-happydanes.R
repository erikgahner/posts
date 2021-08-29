# R script for "Happy Danes"
# Link: https://erikgahner.dk/2021/happy-danes/

library("tidyverse")
library("ggbump")

# https://www.europeansocialsurvey.org/downloadwizard/
hd_raw <- read_csv("ESS1-9e01_1.csv")

hd <- hd_raw |> 
  mutate(happy = ifelse(happy > 10, NA, happy),
         year = case_when(
           essround == 1 ~ 2002,
           essround == 2 ~ 2004,
           essround == 3 ~ 2006,
           essround == 4 ~ 2008,
           essround == 5 ~ 2010,
           essround == 6 ~ 2012,
           essround == 7 ~ 2014,
           essround == 8 ~ 2016,
           essround == 9 ~ 2018
         ),
         country_name = countrycode::countrycode(cntry, "iso2c", "country.name.en")) 

hd |> 
  group_by(country_name, year) |> 
  summarise(happy = weighted.mean(happy, na.rm = TRUE, w = anweight),
            .groups = "drop") |>
  arrange(desc(happy)) |> 
  mutate(denmark = ifelse(country_name == "Denmark", "No", "Yes")) |> 
  ggplot(aes(happy, fill = denmark)) +
  geom_histogram(bins = 45) +
  theme_minimal() +
  ggthemes::scale_fill_gdocs() +
  theme(legend.position = "none") +
  labs(x = "Weighted average happiness estimate",
       y = NULL)

ggsave("happy_dist.png", width = 6, height = 3)
  

hd_val <- hd |> 
  filter(country_name %in% c("Iceland", "Norway", "Denmark", "Switzerland", "Finland", "Sweden")) |> 
  group_by(country_name, year) |> 
  summarise(happy = weighted.mean(happy, na.rm = TRUE, w = anweight),
            .groups = "drop") |> 
  group_by(year) |> 
  arrange(desc(happy)) %>%
  mutate(rank = rank(-happy, ties.method = "random")) 

hd_val |> 
  ggplot(aes(year, rank, color = country_name)) +
  geom_bump(size = 2, smooth = 8) +
  geom_point(size = 4) +
  geom_text(data = hd_val %>% filter(year == 2002),
            aes(x = year - 1, label = country_name), size = 3, hjust = 1) +
  geom_text(data = hd_val %>% filter(year == 2018),
            aes(x = year + 1, label = country_name), size = 3, hjust = 0) +
  scale_x_continuous(limits = c(1998, 2022),
                     breaks = seq(2002, 2018, 2)) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(y = NULL,
       x = NULL) +
  scale_y_reverse(labels = NULL) +
  ggthemes::scale_color_gdocs()

ggsave("happy_bump.png", width = 6, height = 3)

