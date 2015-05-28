# R script til figurer i "Formidlingen af meningsmålinger i valgkampen"
# Link: http://erikgahner.dk/2015/05/28/formidlingen-af-meningsmaalinger-i-valgkampen/

# Åben pakker. Kan installeres med install.packages("")
library(tidyr)
library(ggplot2)
library(gridExtra)
library(downloader)

# Download data
download("https://raw.githubusercontent.com/erikgahner/polls/master/polls.csv", "polls.csv", mode = "wb")
polls <- read.csv("polls.csv")

# Udregn statistisk usikkerhed for alle partierne med et 95% konfidensniveau
polls$ci_a <- 1.96 * sqrt( (polls$party_a * (100-polls$party_a) ) / polls$n)  
polls$ci_b <- 1.96 * sqrt( (polls$party_b * (100-polls$party_b) ) / polls$n)  
polls$ci_c <- 1.96 * sqrt( (polls$party_c * (100-polls$party_c) ) / polls$n)  
polls$ci_f <- 1.96 * sqrt( (polls$party_f * (100-polls$party_f) ) / polls$n)  
polls$ci_i <- 1.96 * sqrt( (polls$party_i * (100-polls$party_i) ) / polls$n)  
polls$ci_k <- 1.96 * sqrt( (polls$party_k * (100-polls$party_k) ) / polls$n)  
polls$ci_o <- 1.96 * sqrt( (polls$party_o * (100-polls$party_o) ) / polls$n)  
polls$ci_v <- 1.96 * sqrt( (polls$party_v * (100-polls$party_v) ) / polls$n)  
polls$ci_oe <- 1.96 * sqrt( (polls$party_oe * (100-polls$party_oe) ) / polls$n)  
polls$ci_aa <- 1.96 * sqrt( (polls$party_aa * (100-polls$party_aa) ) / polls$n)

# Udregn opbakning til blokkene
polls$blue <- polls$party_v+polls$party_c+polls$party_o+polls$party_i+polls$party_k
polls$red <- polls$party_a+polls$party_b+polls$party_f+polls$party_oe+polls$party_aa

# Lav variable med partinavne
polls$Socialdemokraterne <- polls$party_a
polls$Radikale <- polls$party_b
polls$Konservative <- polls$party_c
polls$SF <- polls$party_f
polls$LA <- polls$party_i
polls$Kristendemokraterne <- polls$party_k
polls$DF <- polls$party_o
polls$Venstre <- polls$party_v
polls$Enhedslisten <- polls$party_oe
polls$Alternativet <- polls$party_aa

# Lav data frame med de tre meningsmålinger fra valgkampens første dag
fv15.day1 <- polls[polls$year == 2015 & polls$month == 05 & polls$day == 27,]

# Lav data til langt format
fv15.day1 <- gather(fv15.day1, parti, stemmer, Socialdemokraterne:Alternativet)
fv15.day1 <- gather(fv15.day1, partic, ci, ci_a:ci_aa)

# Lav figur med opbakningen til partierne
png('fv15_dag1_partierne.png', height=6, width=6, units='in', res=200)
ggplot(fv15.day1, aes(x=pollingfirm, y=stemmer)) + 
  geom_errorbar(width=0, aes(ymin=stemmer-ci, ymax=stemmer+ci)) +
  geom_point(size=2) + 
  facet_wrap( ~ parti, ncol = 3) +
  ggtitle("Partiernes opbakning i tre målinger, 27. maj, 2015") +
  ylab("Stemmer (%)") +
  xlab("") +
  theme_minimal()
dev.off()

# Brug meningsmålingerne fra den første dag i valgkampen
fv15.day1.blok <- polls[polls$year == 2015 & polls$month == 05 & polls$day == 27,]

# Udregn statistisk usikkerhed med et 95% konfidensniveau
fv15.day1.blok$ci_blue <- 1.96 * sqrt( (fv15.day1.blok$blue * (100-fv15.day1.blok$blue) ) / fv15.day1.blok$n)  
fv15.day1.blok$ci_red <- 1.96 * sqrt( (fv15.day1.blok$red * (100-fv15.day1.blok$red) ) / fv15.day1.blok$n)

# Gem figur til blå blok
plot.blue <- ggplot(fv15.day1.blok, aes(x=pollingfirm, y=blue)) + 
  geom_errorbar(width=0, colour="blue", aes(ymin=blue-ci_blue, ymax=blue+ci_blue)) +
  geom_point(size=4, colour="blue") + 
  ggtitle("Blå blok") +
  ylab("Stemmer (%)") +
  xlab("") +
  theme_minimal()

# Gem figur til rød blok
plot.red <- ggplot(fv15.day1.blok, aes(x=pollingfirm, y=red)) + 
  geom_errorbar(width=0, colour="red", aes(ymin=red-ci_red, ymax=red+ci_red)) +
  geom_point(size=4, colour="red") + 
  ggtitle("Rød blok") +
  ylab("Stemmer (%)") +
  xlab("") +
  theme_minimal()

# Lav figur med opbakningen til blokkene
png('fv15_dag1_blok.png', height=3, width=5, units='in',res=200)
grid.arrange(plot.blue, plot.red, ncol=2)
dev.off()