# R script to "At what age are people considered old? #2"
# Link: https://erikgahner.dk/2022/at-what-age-are-people-considered-old-2/

library("tidyverse")
library("haven")

ess <- read_dta("ESS4e04_5.dta")

ess <- ess %>%
  mutate(
    age = ifelse(agea < 18 | agea > 100, NA, agea),
    old = ifelse(agdcold < 18 | agdcold > 100, NA, agdcold)
  )

ggplot(ess, aes(x=age, y=old)) +
  geom_jitter(colour="gray", alpha=0.7, shape=1, size=.5) +
  stat_function(fun = function(x) x, geom="line", linetype="dashed") +
  geom_smooth(method = "loess", se=FALSE, size = 2, colour="#4678B0") +
  stat_function(fun = function(x) sqrt(x) * 8, geom="line", colour="#D77F3E", size = 2) +
  stat_function(fun = function(x) 50 + sqrt(x) * 2, geom="line", colour="#FF4136", size = 2) +
  theme_minimal() +
  annotate("text", x = 95, y = 22, colour="#D77F3E", parse = TRUE, label = as.character(expression("sqrt(age) %*% 8"))) +
  annotate("text", x = 92.3, y = 15, colour="#FF4136", parse = TRUE, label = as.character(expression("50 + sqrt(age) %*% 2"))) +
  labs(x = "Age of respondent",
       y = "Age people start being described as old")

ggsave("ess-old_eq.png", width=6, height=4)

reg_age <- lm(old ~ age, data = ess)

summary(reg_age)
