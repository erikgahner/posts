# R script for "National narcissism and proportion estimation in surveys"
# URL: https://erikgahner.dk/2023/national-narcissism-and-proportion-estimation-in-surveys/

library("tidyverse")
library("haven")

# Data available at: https://osf.io/z45hf/
df <- read_sav("OSF_World_History_Survey_Data.sav")

df |> 
  mutate(country = labelled::to_factor(country)) |> 
  group_by(country) |> 
  summarise(Mean = mean(ContribHist),
            Median = median(ContribHist)) |> 
  mutate(country = fct_reorder(country, Mean)) |> 
  pivot_longer(Mean:Median) |> 
  mutate(value = value / 100) |> 
  ggplot(aes(country, value)) +
  geom_line(color="#E7E7E7", linewidth=3.5) + 
  geom_point(aes(colour = name), size = 3) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.y = element_text(color="black"),
        axis.text.x = element_text(color="#989898"),
        axis.title = element_blank(),
        panel.grid = element_blank()
  ) +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
  scale_colour_manual(values = c("#FF4136", "#0074D9")) +
  labs(x = NULL,
       y = NULL,
       colour = NULL)

ggsave("worldhistory.png", width = 4, height = 6, bg = "white")

