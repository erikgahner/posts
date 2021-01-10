# R script for figure in "How to improve your figures #2: Don’t show overlapping text labels"
# Link: https://erikgahner.dk/2021/how-to-improve-your-figures-2-dont-show-overlapping-text-labels/

library("tidyverse")
library("haven")

# Replication data file for: https://doi.org/10.1177%2F0010414020912262
bureaucracygrowth <- read_dta("22725104_Replication_data_Bureaucracy_Growth.dta")

bureaucracygrowth %>% 
  mutate(country_name_show = case_when(
    v2stcritrecadmv9 < -1  ~ country_name,
    QoG_expert_q2_a > 6.3 | QoG_expert_q2_a < 2 ~ country_name,
    v2stcritrecadmv9 > 0.7 & QoG_expert_q2_a < 3.5 ~ country_name,
    v2stcritrecadmv9 < 1 & QoG_expert_q2_a > 4.4 ~ country_name,
    TRUE ~ ""
  )) %>% 
  ggplot(aes(v2stcritrecadmv9, QoG_expert_q2_a)) +
  geom_smooth(method = "lm", se = FALSE) +
  ggrepel::geom_text_repel(aes(label = country_name_show)) +
  geom_point() +
  theme_minimal() +
  labs(y = "Meritocratic recruitment (QoG expert-survey), 2014",
       x = "Meritocratic recruitment (V-Dem), 2014")

ggsave("bureaucracygrowth.png", width = 6, height = 6)