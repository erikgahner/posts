# R script for "Statistisk usikkerhed i meningsmålingerne"
# URL: https://erikgahner.dk/2022/statistisk-usikkerhed-i-meningsmalingerne/

library("tidyverse")

set.seed(1234)

fv <- data.frame(
  parti = c("Socialdemokratiet", "Radikale Venstre", "Det Konservative Folkeparti",
            "Nye Borgerlige", "Klaus Riskær Pedersen", "SF", "Liberal Alliance",
            "Kristendemokraterne", "Dansk Folkeparti", "Stram Kurs", "Venstre", 
            "Enhedslisten", "Alternativet", "Uden for partierne"),
  stemmer = c(915393, 304273, 233349, 83228, 29617, 272093, 82228, 61215,
              308219, 63091, 825486, 244664, 104148, 2755)
)

sum(fv$stemmer)

fv_alle <- data.frame(valg = c(
  rep("Socialdemokratiet", 915393),
  rep("Radikale Venstre", 304273),
  rep("Det Konservative Folkeparti", 233349),
  rep("Nye Borgerlige", 83228),
  rep("Klaus Riskær Pedersen", 29617),
  rep("SF", 272093),
  rep("Liberal Alliance", 82228),
  rep("Kristendemokraterne", 61215),
  rep("Dansk Folkeparti", 308219),
  rep("Stram Kurs", 63091),
  rep("Venstre", 825486),
  rep("Enhedslisten", 244664),
  rep("Alternativet", 104148),
  rep("Uden for partierne", 2755))
  )


for (i in seq_along(1:100)) {
  assign(paste0("valg", i), pull(slice_sample(fv_alle, n = 1000), valg) )
}

fv_maalinger <- data.frame(mget(ls()[str_detect(ls(), "valg")]))

table(fv_maalinger$valg1)

fv_maalinger |> 
  count(valg1)

fv_mean <- fv_maalinger |> 
  pivot_longer(valg1:valg99, names_to = "valg", values_to = "stemme") |> 
  mutate(valg = parse_number(valg)) |> 
  group_by(valg) |> 
  summarise(parti_a = mean(ifelse(stemme == "Socialdemokratiet", 1, 0)),
            parti_v = mean(ifelse(stemme == "Venstre", 1, 0)))


fv_mean |> 
  pivot_longer(starts_with("parti")) |> 
  mutate(linje = ifelse(name == "parti_a", 0.259, 0.234)) |> 
  mutate(name = ifelse(name == "parti_a", "Socialdemokratiet", "Venstre")) |> 
  ggplot(aes(value, fill = name)) +
  geom_histogram(bins = 25, alpha = 0.6) +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_vline(aes(xintercept = linje), linetype = "dashed") +
  facet_wrap(~ name, scales = "free_x") +
  scale_fill_manual(values = c("#E3515D", "#459BC8")) +
  theme(legend.position = "none") +
  labs(x = NULL,
       y = NULL)

ggsave("usikkerhed_a_v_histogram.png", width = 5, height = 2)


fv_mean |> 
  mutate(ci = 1.96 * sqrt((parti_a * (1 - parti_a)) / 1000)) |> 
  mutate(inkluderet = case_when(
    parti_a + ci < 0.259 & parti_a - ci < 0.259 ~ "Ude",
    parti_a + ci > 0.259 & parti_a - ci > 0.259 ~ "Ude",
    TRUE ~ "Inde"
  )) |> 
  mutate(valg = factor(valg)) |> 
  mutate(valg = fct_reorder(valg, parti_a)) |> 
  ggplot(aes(valg, y = parti_a, 
             ymin = parti_a - ci, ymax = parti_a + ci,
             group = inkluderet, colour = inkluderet)) +
  geom_point(size = .5) +
  geom_errorbar(width = 0) +
  geom_hline(yintercept = 0.259) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        panel.grid.major.x = element_blank(),
        legend.position = "none") +
  scale_color_manual(values = c("#001f3f", "#FF4136")) +
  labs(y = NULL)

ggsave("usikkerhed_100.png", width = 5, height = 3)
