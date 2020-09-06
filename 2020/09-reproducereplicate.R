# R script to figures in "Reproduce before you replicate"
# Link: https://erikgahner.dk/2020/reproduce-before-you-replicate/

library("tidyverse")
library("haven")
library("sjPlot")
library("here")
library("broom")

oxley <- read_sav(here("oxley", "science data set.sav"))

reg_oxley_reported <- lm(threat ~ meanampisi + female + Yearborn + demog3 + demog1, data = oxley)
reg_oxley_simple <- lm(threat ~ meanampisi, data = oxley)
reg_oxley_cov_no_edu <- lm(threat ~ meanampisi + female + Yearborn + demog3, data = oxley)
reg_oxley_cov_edu_f <- lm(threat ~ meanampisi + factor(demog1), data = oxley)
reg_oxley_cov_all_f <- lm(threat ~ meanampisi + female + Yearborn + demog3 + factor(demog1), data = oxley)

tab_model(reg_oxley_reported, 
       reg_oxley_simple,
       reg_oxley_cov_no_edu,
       reg_oxley_cov_edu_f,
       reg_oxley_cov_all_f,
       show.ci = FALSE,
       show.p = FALSE,
       collapse.se = TRUE,
       show.se = TRUE,
       show.intercept = FALSE,
       file = "tab-oxley.htm",
       pred.labels = c("Mean amplitude", "Female", "Age", "Income", "Education", 
                       "Edu: trade school", "Edu: some college", "Edu: college grad", 
                       "Edu: college degree plus"),
       dv.labels = c(paste0("(", 1:5, ")")))


naes <- read_dta(here("Levendusky", "waves23_v12.dta"))

naes <- naes %>% 
  # Candidate feeling thermometers 
  mutate(bho_therm2 = ifelse(ABo02_2 < 101, ABo02_2, NA),
         bho_therm3 = ifelse(ABo02_3 < 101, ABo02_3, NA),
         jm_therm2 = ifelse(AAm01_2 < 101, AAm01_2, NA),
         jm_therm3 = ifelse(AAm01_3 < 101, AAm01_3, NA)
         ) %>% 
  # Party ID
  mutate(pid = (-1 * MA01_3) + 8) %>% ## KN reverses PID scale
  mutate(sp = ifelse(pid %in% c(1, 7), 1, 0),
         dem = case_when(
           pid < 4 ~ 1,
           pid > 4 ~ 0,
           TRUE ~ NA_real_
         )) %>% 
  # Pool parties into out-party & same-party effects
  mutate(outparty_ft = ifelse(pid < 4, jm_therm3,
                               ifelse(pid > 4, bho_therm3, NA)),
         outparty_ft2 = ifelse(pid < 4, jm_therm2,
                                ifelse(pid > 4, bho_therm2, NA)),
         sameparty_ft = ifelse(pid > 4, jm_therm3,
                                ifelse(pid < 4, bho_therm3, NA))) %>% 
  # Define Treatments Relative to July 4th
  ## Code up the Interview Date 
  mutate(finish_date = as.Date(as.character(DATE_3), format="%Y%m%d")) %>% 
  ## 14-day window (June 27-July 10 vs. May 30-June 12/August 1-14)
  mutate(window14 = ifelse(((finish_date > "2008-05-29" &
                         finish_date < "2008-06-13") | 
                        (finish_date > "2008-07-31" &
                           finish_date < "2008-08-15")), 0,
                     ifelse(finish_date > "2008-06-26" &
                              finish_date < "2008-07-11", 1, NA))) %>% 
  ## 10-day window (June 29-July 9 vs. June 1-10/August 3-12)
  mutate(window10 = ifelse(((finish_date>"2008-05-31" &
                         finish_date<"2008-06-11") | 
                        (finish_date>"2008-08-02" &
                           finish_date<"2008-08-13")),0,
                     ifelse(finish_date>"2008-06-28" &
                              finish_date<"2008-07-10",1,NA))) %>% 
  ## 7-day window (July 1-7 vs. June 1-7/August 1-7)
  mutate(window7 = ifelse((finish_date>"2008-05-31" &
                       finish_date<"2008-06-08") | 
                      (finish_date>"2008-07-31" &
                         finish_date<"2008-08-08"),0,
                    ifelse(finish_date>"2008-06-30" &
                             finish_date<"2008-07-08",1,NA))) %>% 
  ## 5-day window (July 2-6 vs. June 4-8/August 6-11)
  mutate(window5 = ifelse(((finish_date>"2008-06-03" &
                        finish_date<"2008-06-09") | 
                       (finish_date>"2008-08-05" &
                          finish_date<"2008-08-12")),0,
                    ifelse(finish_date>"2008-07-01" &
                             finish_date<"2008-07-07",1,NA))) %>% 
  ## 3-day window: July 3-5 vs. June 5-7 or August 7-9 
  mutate(window3 = ifelse(((finish_date>"2008-06-04" &
                        finish_date<"2008-06-08") | 
                       (finish_date>"2008-08-06" &
                          finish_date<"2008-08-10")),0,
                    ifelse(finish_date>"2008-07-02" &
                             finish_date<"2008-07-06",1,NA))) %>% 
  ## One Day Window (July 4th vs. June 6th/August 8th)  
  mutate(window1 = ifelse((finish_date=="2008-06-06"| finish_date=="2008-08-08"),0,
                    ifelse(finish_date=="2008-07-04",1,NA))) %>% 
  ## look at days from July 4th (continuous)
  mutate(julyfourth = as.Date("2008-07-04"),
         days_away = abs(finish_date - julyfourth),
         days_squared = as.numeric(days_away)^2,
         wk_away = days_away/7,
         weeks_away = trunc(wk_away),
         days_squared = as.numeric(days_away)^2,
         days_cubed = as.numeric(days_away)^3)
  
  
outparty_1 <- tidy(lm(outparty_ft ~ window14, data = naes)) %>% filter(term == "window14")
outparty_2 <- tidy(lm(outparty_ft ~ window10, data = naes)) %>% filter(term == "window10")
outparty_3 <- tidy(lm(outparty_ft ~ window7, data = naes)) %>% filter(term == "window7")
outparty_4 <- tidy(lm(outparty_ft ~ window5, data = naes)) %>% filter(term == "window5")
outparty_5 <- tidy(lm(outparty_ft ~ window3, data = naes)) %>% filter(term == "window3")
outparty_6 <- tidy(lm(outparty_ft ~ window1, data = naes)) %>% filter(term == "window1")

outparty_w3 <- rbind(outparty_1, outparty_2, outparty_3,
                  outparty_4, outparty_5, outparty_6) %>% 
  mutate(wave = "Wave 3 (Table 4, Levendusky 2018)")

outparty_w2_1 <- tidy(lm(outparty_ft2 ~ window14, data = naes)) %>% filter(term == "window14")
outparty_w2_2 <- tidy(lm(outparty_ft2 ~ window10, data = naes)) %>% filter(term == "window10")
outparty_w2_3 <- tidy(lm(outparty_ft2 ~ window7, data = naes)) %>% filter(term == "window7")
outparty_w2_4 <- tidy(lm(outparty_ft2 ~ window5, data = naes)) %>% filter(term == "window5")
outparty_w2_5 <- tidy(lm(outparty_ft2 ~ window3, data = naes)) %>% filter(term == "window3")
outparty_w2_6 <- tidy(lm(outparty_ft2 ~ window1, data = naes)) %>% filter(term == "window1")

outparty_w2 <- rbind(outparty_w2_1, outparty_w2_2, outparty_w2_3,
                  outparty_w2_4, outparty_w2_5, outparty_w2_6) %>% 
  mutate(wave = "Wave 2 (Placebo test)")


outparty_1_wc <- tidy(lm(outparty_ft ~ window14 + outparty_ft2, data = naes)) %>% filter(term == "window14")
outparty_2_wc <- tidy(lm(outparty_ft ~ window10 + outparty_ft2, data = naes)) %>% filter(term == "window10")
outparty_3_wc <- tidy(lm(outparty_ft ~ window7 + outparty_ft2, data = naes)) %>% filter(term == "window7")
outparty_4_wc <- tidy(lm(outparty_ft ~ window5 + outparty_ft2, data = naes)) %>% filter(term == "window5")
outparty_5_wc <- tidy(lm(outparty_ft ~ window3 + outparty_ft2, data = naes)) %>% filter(term == "window3")
outparty_6_wc <- tidy(lm(outparty_ft ~ window1 + outparty_ft2, data = naes)) %>% filter(term == "window1")

outparty_w3_wc <- rbind(outparty_1_wc, outparty_2_wc, outparty_3_wc,
                     outparty_4_wc, outparty_5_wc, outparty_6_wc) %>% 
  mutate(wave = "Wave 3 + Wave 2 outcome covariate")

outparty <- rbind(outparty_w3, outparty_w2, outparty_w3_wc) %>% 
  mutate(Window = as.factor(ifelse(term == "window1", "1 day", 
                         paste0(parse_number(term), " days")))) %>% 
  select(Window, estimate, se = std.error, wave)
  
outparty %>% 
  mutate(Window = fct_relevel(Window, c("1 day", 
                                        "3 days",
                                        "5 days",
                                        "7 days",
                                        "10 days",
                                        "14 days"))) %>% 
  ggplot(aes(x = Window, 
             y = estimate, 
             ymin = estimate - 1.96 * se,
             ymax = estimate + 1.96 * se)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  geom_point() +
  geom_errorbar(width = 0) + 
  facet_wrap(~ wave) +
  theme_bw() +
  coord_flip() +
  labs(y = "Estimate (w. 95% CI)")

ggsave("levendusky.png", width = 8, height =4)
