# R script til figur i "Hvordan klarer Alternativet sig i meningsmålingerne?"
# http://erikgahner.dk/2015/05/07/hvordan-klarer-alternativet-sig-i-meningsmaalingerne/

# Åben pakker. Kan installeres med install.packages("")
library(downloader)
library(ggplot2)

# Download fil fra GitHub
download("https://raw.githubusercontent.com/erikgahner/polls/master/polls.csv", "polls.csv", mode = "wb")
polls <- read.csv("polls.csv")

# Lav datovariabel
polls$date <- format(as.Date(c(paste(polls$year, polls$month, polls$day, sep="-")), by = "days"))

# Brug kun meningsmålinger i perioden mellem 24. februar 2015 og 5. maj 2015. 
# Dette kan frit ændres, hvis man ønsker at inkludere nyere meningsmålinger.
polls <- polls[polls$date > as.Date("2015-02-24") & polls$date < as.Date("2015-05-07"),]

# Udregn 95% konfidensintervaller
polls$se_aa <- 1.96 * sqrt( (polls$party_aa * (100-polls$party_aa) ) / polls$n)  

# Lav Institut variabel (alternativt kan legend title på pollingfirm ændres)
polls$Institut <- polls$pollingfirm

# Lav figur. Slet png() og dev.off() hvis det blot skal vises i R
png('alternativet.png', height=3, width=7, units="in", res=200)
ggplot(polls, aes(x=as.Date(date), y=party_aa, colour=Institut)) + 
  geom_errorbar(aes(ymin=party_aa-se_aa, ymax=party_aa+se_aa)) +
  geom_point() + 
  ylab("Stemmer (%)") +
  xlab("") +
  geom_hline(yintercept=2, colour="gray40") +
  theme_minimal() +
  theme(legend.direction = "horizontal", legend.position = "bottom")
dev.off()
