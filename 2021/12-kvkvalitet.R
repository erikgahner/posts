# R script for "Kvaliteten af meningsmålingerne ved kommunalvalget #2"
# URL: https://erikgahner.dk/2021/kvaliteten-af-meningsmalingerne-ved-kommunalvalget-2/

library("tidyverse")
library("lubridate")
library("gt")

kvpolls_raw <- read_csv("https://raw.githubusercontent.com/erikgahner/kvpolls/main/kvpolls.csv") 

maaned <- Vectorize(function(n) c("jan", "feb", "mar", 
                                  "apr", "maj", "jun", "jul", 
                                  "aug", "sep", "okt",
                                  "nov", "dec")[n])

kvpolls <- kvpolls_raw |> 
  mutate(forskel = abs(opbakning - valg_2021)) |> 
  filter(dato > as.Date("2021-10-15"))

kvpolls |>
  select(opbakning, contains("_2021")) |>
  na.omit() |> 
  pivot_longer(contains("_2021")) |> 
  mutate(name = ifelse(name == "valg_2021", "Lokal måling", "Valgvindsprognose")) |> 
  ggplot(aes(opbakning, value, colour = name)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  ggthemes::scale_color_gdocs() +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(colour = NULL,
       x = "Estimat (%)",
       y = "Resultat (%)")

ggsave("kv21_vurdering.png", width = 4, height = 4)

kvpolls |> 
  group_by(kommune, institut, n, dato) %>%
  summarise(fejl = mean(forskel, na.rm = TRUE), .groups = "drop") |> 
  mutate(dato = paste0(day(dato), ". ", maaned(month(dato)))) |> 
  arrange(-fejl) |> 
  gt() |> 
  tab_header(
    title = "Gennemsnitlige fejl i meningsmålingerne",
    subtitle = "Målinger foretaget senest en måned inden kommunalvalget"
  ) |> 
  fmt_number(
    columns = fejl,
    dec_mark = ",",
    sep_mark = ".",
    decimals = 2
  ) |> 
  gtExtras::gt_theme_espn() |> 
  data_color(
    columns = fejl,
    colors = scales::col_numeric(
      palette = c(
        "green", "orange", "red"),
      domain = c(0.5, 2.5))
  ) |> 
  as_raw_html()

kvpolls |> 
  filter(kommune == "København", institut == "Gallup")

kvpolls |> 
  filter(kommune == "Bornholm", institut == "Jysk Analyse")

kvpolls |> 
  filter(kommune == "Frederiksberg", institut == "Gallup")