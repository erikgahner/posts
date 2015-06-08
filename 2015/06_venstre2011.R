# R script til figur i "Kan meningsmålingerne have påvirket Venstres valg i 2011?"
# Link: http://erikgahner.dk/2015/06/08/kan-meningsmaalingerne-have-paavirket-venstres-valg-i-2011/

# Åben pakker. Kan installeres med install.packages("")
library(downloader)
library(ggplot2)

# Download fil fra GitHub
download("https://raw.githubusercontent.com/erikgahner/polls/master/polls.csv", "polls.csv", mode = "wb")

# Hent polls.csv
polls <- read.csv("polls.csv")

# Lav dato variabel
polls$date <- format(as.Date(c(paste(polls$year, 
                                     polls$month, 
                                     polls$day,
                                     sep="-")), by = "days"))

# Brug kun meningsmålinger fra valgkampen 2011
polls <- polls[polls$date < as.Date("2011-09-16") & polls$date > as.Date("2011-08-25"),]

# Lav Institut-variabel (nem måde at ændre legend title)
polls$Institut <- polls$pollingfirm

# Udregn statisisk usikkerhed for Venstre
polls$ci_v <- 1.96 * sqrt( (polls$party_v * (100-polls$party_v) ) / polls$n)  

# Tegn og gem figur
png('venstre2011.png', height=4, width=6, units='in', res=200)
ggplot(polls, aes(x=as.Date(date), y=party_v)) + 
  geom_smooth(aes(ymin=party_v-ci_v, ymax=party_v+ci_v), colour="blue", fill="blue", alpha=0.2) +
  geom_point(size=4, aes(colour=Institut)) + 
  ggtitle("Venstres opbakning under valgkampen i 2011") +
  ylab("Stemmer (%)") +
  xlab("") +
  theme_minimal()
dev.off()
