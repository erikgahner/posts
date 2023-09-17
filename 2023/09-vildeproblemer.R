# R script for "Vilde problemer i meningsmålinger"
# URL: https://erikgahner.dk/2023/vilde-problemer-i-meningsmalinger/

library("tidyverse")
library("ggflags")
library("haven")
library("countrycode")
library("labelled")

df_raw <- read_sav("ZA7953_v1-0-0.sav")

df <- df_raw |> 
  mutate(isocntry = ifelse(isocntry %in% c("DE-E", "DE-W"), "DE", isocntry)) |> 
  transmute(country = countrycode(isocntry, origin = "iso2c", destination = "country.name"),
            stfdem = sd18a,
            isocntry = str_to_lower(isocntry)) |> 
  drop_na(country) |> 
  filter(stfdem != 5) |> 
  mutate(stfdem = to_factor(stfdem)) |> 
  add_count(country, name = "total") |> 
  count(country, stfdem, isocntry, total, name = "values") |> 
  mutate(values_rel = values / total)

df_country <- df |> 
  filter(stfdem == "Very satisfied") |> 
  arrange(-values_rel) |> 
  mutate(rank = row_number())

df |> 
  left_join(select(df_country, country, rank), by = "country") |> 
  mutate(country = fct_reorder(country, -rank)) |> 
  ggplot(aes(x = country, y = values_rel, group = stfdem, fill = stfdem)) +
  geom_col() + 
  coord_flip() +
  geom_flag(aes(country = isocntry, x = country, y = -0.04, size = 2)) +
  scale_size(guide = "none") +
  scale_fill_manual(NULL, guide = guide_legend(reverse = TRUE, ncol = 2),
                      values = c("#0074D9", "#7FDBFF", "#FF851B", "#FF4136")) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = NULL,
       y = NULL) 

ggsave("stfdem_eurobarometer.png", width = 5, height = 8, bg = "white")


df_holdning <- data.frame(question = c("Det danske demokrati løser de store problemer, der optager borgerne",
                              "Det danske demokrati baserer sig på grundige samtaler og argumenter forud for de politiske beslutninger.",
                              "Det danske demokrati balancerer forskellige samfundsgruppers interesser, så en gruppe ikke sætter sig på magten",
                              "Det danske demokrati sikrer, at borgernes grundlovssikrede frihedsrettigheder ikke tilsidesættes af staten.",
                              "Det danske demokrati skaber mulighed for, at alle uanset baggrund (fx køn, religion, social baggrund mv.) kan deltage på lige fod."),
                 value = c(5.2, 5.24, 5.36, 6.09, 6.15))

df_holdning |> 
  mutate(question = str_wrap(question, 20)) |> 
  mutate(question = fct_reorder(question, value)) |> 
  ggplot(aes(question, value, label = value)) +
  geom_point(size = 3) +
  geom_label(nudge_x = 0.25) +
  scale_y_continuous(breaks = 0:10, limits = c(0, 10)) +
  labs(y = NULL,
       x = NULL) +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  annotate(geom="text", x = 1, y = 10, label = "- Passer fuldstændig",
           color="grey40") +
  annotate(geom="text", x = 0.92, y = 0.05, label = "- Passer slet ikke",
           color="grey40")

ggsave("stfdem_holdning.png", width = 7, height = 4, bg = "white")
