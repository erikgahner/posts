# R script for "Hvor gode var meningsmålingerne om forsvarsforbeholdet?"
# URL: https://erikgahner.dk/2022/hvor-gode-var-meningsmalingerne-om-forsvarsforbeholdet/

library("tidyverse")

ff_raw <- read_csv("https://raw.githubusercontent.com/erikgahner/Politologi/master/folkeafstemninger/2022_forsvarsforbeholdet/forsvarsforbeholdet.csv")
rf_raw <- read_csv("https://raw.githubusercontent.com/erikgahner/Politologi/master/folkeafstemninger/2015_retsforbeholdet/retsforbeholdet.csv")

ff <- ff_raw |> 
  filter(str_detect(str_to_lower(spoergsmaal_ordlyd), "forbehold")) |> 
  mutate(n = ifelse(is.na(n), 1000, n),
         svar_vedikke = ifelse(is.na(svar_vedikke), 0, svar_vedikke)) |> 
  mutate(n = n - n * (svar_vedikke / 100)) |> 
  mutate(across(c("svar_ja", "svar_nej"), ~ ifelse(!is.na(svar_andet), (.x / (100 - svar_andet - svar_vedikke))*100, 
                                                   (.x / (100 - svar_vedikke))*100))) |> 
  select(-svar_andet, -svar_vedikke, -starts_with("spoergsmaal_"), -dato_start, -kilde) |> 
  mutate(resultat_ja = 66.9,
         resultat_nej = 33.1) |> 
  mutate(forskel_ja = resultat_ja - svar_ja) |> 
  mutate(usikkerhed = 1.96 * sqrt((svar_ja * (100 - svar_ja)) / n))

rf <- rf_raw |> 
  mutate(n = ifelse(is.na(n), 1000, n),
         svar_vedikke = ifelse(is.na(svar_vedikke), 0, svar_vedikke)) |> 
  mutate(n = n - n * (svar_vedikke / 100)) |> 
  mutate(across(c("svar_ja", "svar_nej"), ~ ifelse(!is.na(svar_andet), (.x / (100 - svar_andet - svar_vedikke))*100, 
                                                   (.x / (100 - svar_vedikke))*100))) |> 
  select(-svar_andet, -svar_vedikke, -starts_with("spoergsmaal"), -dato_start) |> 
  mutate(resultat_ja = 46.9,
         resultat_nej = 53.1) |> 
  mutate(forskel_ja = resultat_ja - svar_ja) |> 
  mutate(usikkerhed = 1.96 * sqrt((svar_ja * (100 - svar_ja)) / n))

ff |> 
  ggplot(aes(dato_end, forskel_ja)) +
  geom_point() 

ff |> 
  filter(dato_end > as.Date("2022-06-01") - 10) |> 
  group_by(institut) %>%
  slice(which.max(dato_end)) |>
  ungroup() |> 
  mutate(institut = fct_reorder(institut, forskel_ja)) |> 
  ggplot(aes(institut, forskel_ja, ymin = forskel_ja - usikkerhed, ymax = forskel_ja + usikkerhed)) +
  geom_col(fill = "lightgrey") +
  geom_errorbar(width = 0) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(x = NULL,
       title = "Folkeafstemningen om forsvarsforbeholdet",
       y = "Forskel mellem seneste måling og valgresultat (%-point)") +
  coord_flip() +
  ylim(-5, 9.5)

ggsave("poll-forsvarsforbeholdet.png", width = 6, height = 3, bg = "white")

rf |> 
  filter(dato_end > as.Date("2015-12-03") - 10) |> 
  group_by(institut) %>%
  slice(which.max(dato_end)) |>
  ungroup() |> 
  mutate(institut = fct_reorder(institut, -forskel_ja)) |> 
  ggplot(aes(institut, forskel_ja, ymin = forskel_ja - usikkerhed, ymax = forskel_ja + usikkerhed)) +
  geom_col(fill = "lightgrey") +
  geom_errorbar(width = 0) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(x = NULL,
       title = "Folkeafstemningen om retsforbeholdet",
       y = "Forskel mellem seneste måling og valgresultat (%-point)") +
  coord_flip() +
  ylim(-5, 9.5)

ggsave("poll-retsforbeholdet.png", width = 6, height = 3, bg = "white")

polls <- ff_raw |> 
  mutate(n = ifelse(is.na(n), 1000, n),
         svar_vedikke = ifelse(is.na(svar_vedikke), 0, svar_vedikke)) |> 
  filter(str_detect(spoergsmaal_ordlyd, "forbehold")) |> 
  mutate(poll_date = as.Date(dato_end)) |> 
  mutate(poll = paste0(institut, "-", poll_date)) |> 
  mutate(n = n - n * (svar_vedikke / 100)) |> 
  mutate(across(c("svar_ja", "svar_nej"), ~ ifelse(!is.na(svar_andet), (.x / (100 - svar_andet - svar_vedikke))*100, 
                                                   (.x / (100 - svar_vedikke))*100))) |> 
  mutate(across(c("svar_ja", "svar_nej"), ~ .x / 100)) |> 
  transmute(pollingfirm = institut, poll_date, svar_ja, svar_nej, n = round(n)) |> 
  mutate(pollingfirm = fct_drop(pollingfirm)) |>
  as.data.frame() 

ffsnit <- read_csv("ffsnit.csv")

ffsnit %>% 
  filter(party_name == "Ja") %>% 
  mutate(est = est * 100,
         lower = lower * 100,
         upper = upper * 100) %>% 
  ggplot(aes(x = dato, y = est)) +
  theme_minimal(base_size = 12) %+replace% 
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_line(colour = "grey20", size = 0.3, linetype="dotted"),
        panel.grid.minor.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none"
  ) +
  geom_line(colour = "black", size = 0.8) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "gray", alpha = .3) +
  labs(y = "Opbakning til Ja-siden (%)",
       x = NULL) +
  scale_x_date(date_breaks = "1 month", 
               labels = scales::date_format("%B")) +
  geom_vline(xintercept = as.Date("2022-06-01"), linetype = "dotted", col = "#0074D9") +
  geom_point(data = poll, aes(x = poll_date, y = svar_ja * 100))

ggsave("ffsnit.png", width = 8, height = 4)