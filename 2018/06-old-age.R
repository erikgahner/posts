# R script to "At what age are people considered old?"
# Link: http://erikgahner.dk/2018/at-what-age-are-people-considered-old/

library("tidyverse")
library("rio")

ess <- import("ESS4e04_4.dta")

ess <- ess %>%
  mutate(
    age = ifelse(agea < 18 | agea > 100, NA, agea),
    old = ifelse(agdcold < 18 | agdcold > 100, NA, agdcold)
    )

ggplot(ess, aes(x=age, y=old)) +
  geom_jitter(colour="gray", alpha=0.7, shape=1, size=.5) +
  stat_function(fun = function(x) x, geom="line", linetype="dashed") +
  stat_function(fun = function(x) sqrt(x) * 8, geom="line", colour="#D77F3E", size = 2) +
  geom_smooth(method = "loess", se=FALSE, size = 2, colour="#4678B0") +
  theme_minimal() +
  labs(x = "Age of respondent",
       y = "Age people start being described as old")

ggsave("ess-old.png", width=6, height=4)
  
mean(ess$old[ess$age == 18], na.rm=TRUE)
median(ess$old[ess$age == 18], na.rm=TRUE)

mean(ess$old[ess$age == 25], na.rm=TRUE)
median(ess$old[ess$age == 25], na.rm=TRUE)
