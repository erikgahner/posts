# R script to "Eurobarometer and Euroscepticism"
# Link: https://erikgahner.dk/2019/eurobarometer-and-euroscepticism/

library("tidyverse")
library("haven")

eb_raw <- read_dta("ZA6963_v1-0-0.dta")

eb <- eb_raw %>% 
  mutate(responserate = case_when(
    isocntry == "AL" ~ NA_real_,
    isocntry == "FR" ~ 31,
    isocntry == "BE" ~ 47,
    isocntry == "NL" ~ 81,
    isocntry == "DE-E" | isocntry == "DE-W" ~ 15,
    isocntry == "IT" ~ 22,
    isocntry == "LU" ~ 20,
    isocntry == "DK" ~ 28,
    isocntry == "IE" ~ 33,
    isocntry == "GB-GBN" ~ 27,
    isocntry == "GB-NIR" ~ 27,
    isocntry == "GR" ~ 28,
    isocntry == "ES" ~ 34,
    isocntry == "PT" ~ 40,
    isocntry == "FI" ~ 14,
    isocntry == "SE" ~ 75,
    isocntry == "AT" ~ 41,
    isocntry == "CY" | isocntry == "CY-TCC" ~ 43,
    isocntry == "CZ" ~ 42,
    isocntry == "EE" ~ 42,
    isocntry == "HU" ~ 54,
    isocntry == "LV" ~ 38,
    isocntry == "LT" ~ 44,
    isocntry == "MT" ~ 47,
    isocntry == "PL" ~ 42,
    isocntry == "SK" ~ 65,
    isocntry == "SI" ~ 45,
    isocntry == "BG" ~ 49,
    isocntry == "RO" ~ 62,
    isocntry == "TR" ~ NA_real_,
    isocntry == "HR" ~ 42,
    isocntry == "MK" ~ NA_real_,
    isocntry == "ME" ~ NA_real_,
    isocntry == "RS" ~ NA_real_
  )) %>% 
  mutate(attachedEU = ifelse(qd1a_3 == 5, NA, 4 - qd1a_3))

eb_agg <- eb %>% 
  mutate(isocntry = case_when(
    isocntry == "DE-E" ~ "DE", isocntry == "DE-W" ~ "DE",
    isocntry == "GB-GBN" ~ "GB", isocntry == "GB-NIR" ~ "GB",
    TRUE ~ isocntry
  )) %>% 
  group_by(isocntry) %>% 
  summarise(responserate = mean(responserate, na.rm = TRUE),
            attachedEU = mean(attachedEU, na.rm = TRUE)) %>% 
  drop_na(responserate)

eb_agg %>% 
  ggplot(aes(responserate, attachedEU)) +
  geom_smooth(se = FALSE, method = "lm") +
  geom_text(aes(label = isocntry)) +
  theme_minimal() +
  labs(y = "Attachment to EU",
       x = "Response rate (%)") 
ggsave("eb_agg.png", height = 6, width = 6)

ess <- rio::import("ESS9e01.dta", haven = FALSE)

ess_agg <- ess %>% 
  group_by(cntry) %>% 
  summarise(uniEU = mean(euftf, na.rm = TRUE)) %>% 
  mutate(isocntry = cntry)

eb_ess <- left_join(eb_agg, ess_agg, by = "isocntry")

eb_ess %>% 
  select_if(is.numeric) %>% 
  cor(use="pairwise.complete.obs")

eb_ess %>% 
  ggplot(aes(attachedEU, uniEU)) +
  theme_minimal() +
  geom_smooth(se = FALSE, method = "lm") +
  geom_point(aes(size = responserate)) + 
  labs(y = "European unification (ESS)", 
       x = "Attachment to EU (Eurobarometer)",
       size = "Response rate") 
  
ggsave("eb_ess.png", height = 6, width = 6)