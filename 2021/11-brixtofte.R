# R script for "Brixtofte-skandalens effekt på den offentlige opinion"
# Link: https://erikgahner.dk/2021/brixtofte-skandalens-effekt-pa-den-offentlige-opinion/

library("tidyverse")
library("haven")
library("lubridate")
library("broom")

# Download data from: https://rafil.azurewebsites.net/aabnedata/valgundersoegelser/Valgunders%C3%B8gelsen%202011%20(DDA-27067).zip
dnes_raw <- read_dta("data12516.dta") |> 
  janitor::clean_names()

dnes <- dnes_raw |> 
  mutate(v284 = as.numeric(v284)) |> 
  mutate(
    # Skandale variabel
    scandal = case_when(
      v5 == 12 | v5 == 1 ~ 0,
      v5 == 2 & v6 < 6 ~ 0,
      v5 == 3 ~ 1,
      v5 == 2 & v6 > 6 ~ 1
    ),
    
    # Afhængige variable
    corruption = ifelse(v259 != 8, 4 - v259, NA),
    trust = ifelse(v171 != 8, 4 - v171, NA),
    
    # Kontrolvariable
    male = ifelse(v270 == 1, 1, 0),
    age = v300,
    education = case_when(
      v284 == 9 ~ NA_real_,
      v284 == 6 ~ 0,
      TRUE ~ v284
    ),
    income = ifelse(v273 != 18, v273, NA),
    married = ifelse(v272 == 2, 1, 0),
    ideology = ifelse(v187 == 18, NA, v187),
    urban = ifelse(v282 == 8, NA, v282),
    blue = case_when(
      v22 %in% c(1,2,4,5,10) ~ 0,
      v22 %in% c(3,6,7,8,9) ~ 1
    ),
    date = ymd(paste0(v4, "-", v5, "-", v6))
  )

table(dnes$scandal)

ggplot(dnes, aes(x=date)) +
  geom_histogram(fill="gray80", colour="black", bins = 20) +
  ylab("Respondenter (antal)") + 
  xlab("") +
  theme_bw() +
  geom_vline(xintercept = as.Date("2002-02-06"), colour = "red", linetype = "dashed")

ggsave("brixtofte-sample.png", height = 4, width = 8)

coverage_df <- data.frame(
  keyword = c(rep("Farum & Brixtofte & Venstre", 2),
              rep("Brixtofte & skandale", 2),
              rep("Brixtofte & Venstre & skandale", 2)),
  period = factor(c(rep(c("Før skandale", "Efter skandale"), 3)), 
                  levels = c("Før skandale", "Efter skandale")),
  articles = c(37, 1144, 3, 293, 2, 133)
)

coverage_df |>
  ggplot(aes(period, articles)) +
  geom_col() +
  facet_wrap(~ keyword, scales = "free_y") +
  theme_bw() +
  labs(y = "Artikler (antal)",
       x = NULL)

ggsave("brixtofte-artikler.png", width = 7, height = 3)

reg_corruption_1 <- lm(corruption ~ scandal, data = dnes)
reg_corruption_2 <- lm(corruption ~ scandal + male + age + education + income + married + ideology + urban + blue, data = dnes)
reg_trust_1 <- lm(trust ~ scandal, data = dnes)
reg_trust_2 <- lm(trust ~ scandal + male + age + education + income + married + ideology + urban + blue, data = dnes)

reg_df <- map_dfr(list(reg_corruption_1, reg_corruption_2,
             reg_trust_1, reg_trust_2), 
        tidy, conf.int = TRUE, .id = "model") |> 
  filter(term == "scandal") |> 
  mutate(outcome = ifelse(model %in% c(1, 2), "Korruption", "Tillid"),
         covariate = ifelse(model %in% c(2, 4), "Med kontrolvariable", "Uden kontrolvariable"))
  
reg_df |> 
  ggplot(aes(outcome, estimate, group = covariate, colour = covariate, ymin = conf.low, ymax = conf.high)) +
  geom_point(position = position_dodge(width = .5), size = 2) +
  geom_errorbar(position = position_dodge(width = .5), width = 0, size = .8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  theme(legend.position = "bottom") +
  coord_flip() + 
  labs(x = NULL,
       y = "Effekt",
       colour = NULL) +
  ggthemes::scale_color_gdocs()

ggsave("brixtofte-effekter.png", width = 6, height = 3)
