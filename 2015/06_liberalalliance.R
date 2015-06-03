# R script til figurer i "Her er tre figurer, der alle viser Liberal Alliances opbakning i meningsmålingerne"
# Link: http://erikgahner.dk/2015/06/03/her-er-tre-figurer-der-alle-viser-liberal-alliances-opbakning-i-meningsmaalingerne/

# Åben pakker - kan hentes med install.packages("")
library(downloader)
library(ggplot2)

# Download meningsmålinger
download("https://raw.githubusercontent.com/erikgahner/polls/master/polls.csv", "polls.csv", mode = "wb")
polls <- read.csv("polls.csv")

# Lav datovariabel
polls$date <- format(as.Date(c(paste(polls$year, 
                                     polls$month, 
                                     polls$day,
                                     sep="-")), by = "days"))

# Afgræs perioden
polls <- polls[polls$date > as.Date("2015-05-01") & polls$date < as.Date("2015-06-03"),]

# Udregn statistisk usikkerhed
polls$ci_i <- 1.96 * sqrt( (polls$party_i * (100-polls$party_i) ) / polls$n)  

# Lav Institut-variabel (nem måde at ændre legend title)
polls$Institut <- polls$pollingfirm

# Gem figur med opbakningen til Liberal Alliance i Epinions målinger
png('la_epinion.png', height=5, width=5, units='in', res=200)
ggplot(polls[polls$pollingfirm == "Epinion",], aes(x=as.Date(date), y=party_i)) + 
  stat_smooth(aes(ymin=party_i-ci_i, ymax=party_i+ci_i), colour="orange", se=F) +
  geom_point(size=3, colour="orange") + 
  ggtitle("Liberal Alliances opbakning, Epinion, maj 2015") +
  ylab("Stemmer (%)") +
  xlab("") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank())
dev.off()

# Gem figur med Liberal Alliances opbakning
png('la_alle.png', height=5, width=7, units='in', res=200)
ggplot(polls, aes(x=as.Date(date), y=party_i)) + 
  stat_smooth(aes(ymin=party_i-ci_i, ymax=party_i+ci_i), colour="orange", fill="orange", alpha=0.2) +
  geom_point(size=3, aes(colour=Institut)) + 
  ggtitle("Liberal Alliances opbakning, alle institutter, maj 2015") +
  ylab("Stemmer (%)") +
  xlab("") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank())
dev.off()

