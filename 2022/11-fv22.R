library("tidyverse")

fv_raw <- read_csv("11-fv22.csv")

fv <- fv_raw |> 
  pivot_longer(Politologi:TV2_2020) |> 
  mutate(type = case_when(name %in% c("Politologi", "Altinget") ~ "Vægtet snit", 
                          name %in% c("TV2_2000", "TV2_2020", "DR") ~ "Exitpoll",
                          TRUE ~ "Enkeltmåling")) |> 
  mutate(name = case_when(
    name == "TV2_2000" ~ "TV 2 (20.00)",
    name == "TV2_2020" ~ "TV 2 (20.20)",
    TRUE ~ name
  ))

fv |> 
  mutate(Forskel = value - Resultat) |> 
  mutate(name = fct_reorder2(name, name, type)) |> 
  ggplot(aes(name, Forskel, fill = type)) +
  geom_col() +
  facet_wrap(~ Parti, ncol = 3) +
  coord_flip() +
  scale_fill_manual(values = c("#FFC107", "#F44236", "#2196F3")) +
  scale_y_continuous(labels=function(x) sprintf("%.1f", x)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title = element_blank(),
    strip.text = element_text(face = "bold", color = "grey20"),
    axis.text = element_text(color = "grey40"),
    plot.margin = margin(20, 30, 20, 30),
    plot.title = element_text(margin = margin(0, 0, -100, 0), size = 26, face = "bold", vjust = 0, color = "grey25"),
    plot.caption = element_text(size = 11)
  )

ggsave("poll_evaluering_1.png", width = 6, height = 9, bg = "white")

fv_agg <- fv |> 
  mutate(Forskel = abs(Resultat - value)) |> 
  group_by(name, type) |> 
  summarise(fejl_AME = mean(Forskel),
            fejl_RMSE = sqrt(mean((Resultat - value)^2)),
            .groups = "drop") 

fv_agg |> 
  mutate(name = fct_reorder(name, fejl_AME)) |> 
  ggplot(aes(x = name, y = fejl_AME, fill = type)) +
  geom_col() +
  labs(y = "Gennemsnitlig afvigelse i procentpoint",
       x = "") +
  scale_fill_manual(values = c("#FFC107", "#F44236", "#2196F3")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.title = element_blank(),
    axis.title = element_blank(),
    strip.text = element_text(face = "bold", color = "grey20"),
    axis.text = element_text(color = "grey40"),
    plot.margin = margin(20, 30, 20, 30),
    plot.title = element_text(margin = margin(0, 0, -100, 0), size = 26, face = "bold", vjust = 0, color = "grey25"),
    plot.caption = element_text(size = 11)
  )

ggsave("poll_evaluering_2.png", width = 8, height = 5, bg = "white")

fv_agg |> 
  mutate(name = fct_reorder(name, fejl_RMSE)) |> 
  ggplot(aes(x = name, y = fejl_RMSE, fill = type)) +
  geom_col() +
  labs(y = "Gennemsnitlig afvigelse i procentpoint",
       x = "") +
  scale_fill_manual(values = c("#FFC107", "#F44236", "#2196F3")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.title = element_blank(),
    axis.title = element_blank(),
    strip.text = element_text(face = "bold", color = "grey20"),
    axis.text = element_text(color = "grey40"),
    plot.margin = margin(20, 30, 20, 30),
    plot.title = element_text(margin = margin(0, 0, -100, 0), size = 26, face = "bold", vjust = 0, color = "grey25"),
    plot.caption = element_text(size = 11)
  )

ggsave("poll_evaluering_3.png", width = 8, height = 5, bg = "white")

