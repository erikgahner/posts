# R script til figurer i "Er uligheden i danskernes lykke blevet større?"
# Link: http://erikgahner.dk/2015/07/08/er-uligheden-i-danskernes-lykke-blevet-stoerre/

# Åbn pakker
library(foreign)
library(ggplot2)
library(car)

# CSV-fil med data fra alle seks runder af European Social Survey kan hentes fra: http://www.europeansocialsurvey.org/downloadwizard/
# Følgende variable bliver anvendt: cntry (land), essround (runde), happy (lykkevariablen)
ess <- read.csv("ESS1-6e01_0_F1.csv")

# Reducer datasættet til de observationer, der har en gyldig værdi på lykkevariablen
ess <- subset(ess, happy <= 10)

# Lav data frame med observationer fra Danmark
dk <- subset(ess, cntry == "DK")

# Lav data frame med observationer fra Danmark i runde 1 (2002) og 6 (2012)
dk16 <- subset(dk, essround == 1 | essround == 6)

# Udfør test for varianshomogenitet
leveneTest(happy ~ as.factor(essround), data=dk16, center=mean)

# Lav data frame med standardafvigelserne i lykke i Danmark fra alle seks runder af ESS
lykkeulighed <- data.frame(aar = c(2002,2004,2006,2008,2010,2012),
                           ulighed = c(
                             sd(dk$happy[dk$essround == 1]),
                             sd(dk$happy[dk$essround == 2]),
                             sd(dk$happy[dk$essround == 3]),
                             sd(dk$happy[dk$essround == 4]),
                             sd(dk$happy[dk$essround == 5]),
                             sd(dk$happy[dk$essround == 6])
                             )
                           )

# Gem figur, der viser udviklingen i standardafvigelserne over tid
png('lykkeulighed.png', height=3, width=7, units="in",res=700)
ggplot(lykkeulighed, aes(x=aar, y=ulighed)) + 
  ylim(1,1.8) +
  scale_x_continuous(breaks=lykkeulighed$aar) +
  geom_line(size=1) +
  xlab("") +
  ylab("Ulighed") +
  theme_minimal()
dev.off()

# Importer data til R uden at tage value labels med fra Stata-filen
# Datasættet kan hentes fra følgende link (kræver login): http://www.europeansocialsurvey.org/download.html?file=ESS6e02_1&y=2012
ess6 <- read.dta("ESS6e02_1.dta", convert.factors=FALSE)

# Alternativt kan man bruge datasættet fra ESS Cumulative Data Wizard og blot kigge på observationerne fra round 6
# ess6 <- subset(ess, essround == 6)

# Reducer datasættet til de observationer, der har en gyldig værdi på lykkevariablen
ess6 <- subset(ess6, happy <= 10)

# Lav data frame med gennemsnitligt lykkeniveau for hvert land
lykke <- aggregate(ess6$happy, list(ess6$cntry), mean)

# Giv nye navne på de to variable
names(lykke) <- c("land", "lykke")

# Plot gennemsnitsværdierne
png('lykke_komparativt.png', height=6, width=3, units="in",res=700)
ggplot(lykke, aes(x=lykke, y=reorder(land, lykke))) +
  geom_point() + 
  ylab("") +
  xlab("Lykkegennemsnit") +
  theme_minimal()
dev.off()

# Tilføj 95% konfidensintervaller, jvf. http://erikgahner.dk/2015/07/08/er-uligheden-i-danskernes-lykke-blevet-stoerre/#comment-15651
lykke <- data.frame(lykke,aggregate(ess6$happy, list(ess6$cntry), sd)[2])
lykke <- data.frame(lykke,aggregate(ess6$happy, list(ess6$cntry), length)[2])

names(lykke) <- c("land", "lykke", "sd", "n")

lykke$ci <- 1.96 * lykke$sd / sqrt(lykke$n)

png('lykke_ci.png', height=4, width=7, units="in",res=700)
ggplot(lykke, aes(x=reorder(land, lykke), y=lykke)) +
  geom_point() + 
  geom_errorbar(aes(ymin=lykke-ci, ymax=lykke+ci), width=0) +
  xlab("") +
  ylab("Lykkegennemsnit") +
  theme_minimal()
dev.off()