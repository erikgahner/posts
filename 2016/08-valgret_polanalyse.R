# R script for "Analyse i Politiken: Hvornår får 16-årige lov til at stemme?"
# URL: https://erikgahner.dk/2016/analyse-i-politiken-hvornaar-faar-16-aarige-lov-til-at-stemme/

library("ggplot2")

valgret <- read.csv("me-survey.csv")

valgret$modstander <- NA
valgret$modstander[valgret$valgret.d == 1] <- 0
valgret$modstander[valgret$valgret.d == 0] <- 1
valgret$modstander[valgret$valgret == 2] <- 1

m <- glm(modstander ~ ideology, data = valgret, family = "binomial")

summary(valgret$ideology)
m.pr <- with(m, data.frame(ideology = seq(from = 0, to = 10, length.out = 100)))

m.pr <- cbind(m.pr, predict(m, newdata = m.pr, type = "link", se = TRUE))
m.pr <- within(m.pr, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

png('polanalyse.png', height=4, width=5.5, units="in",res=600)
ggplot(m.pr, aes(x = ideology, y = PredictedProb)) + 
  geom_line(colour="black", size=1.1) + 
  scale_y_continuous("Modstander af lavere \nvalgretsalder, sandsynlighed", limits=c(0.4,1), labels=c("40%", "60%", "80%", "100%")) +
  scale_x_continuous("", breaks=c(0,10), labels=c("Venstre-\norienteret","H?jre-\norienteret")) +
  theme_minimal() + theme(panel.grid.major.x = element_blank())
dev.off()