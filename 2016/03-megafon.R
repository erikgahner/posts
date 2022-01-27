# R script for "Mistede Socialdemokraterne en fjerdedel af deres vælgere?"
# URL: https://erikgahner.dk/2016/mistede-socialdemokraterne-en-fjerdedel-af-deres-vaelgere/

library(downloader)
library(ggplot2)

download("https://raw.githubusercontent.com/erikgahner/polls/master/polls.csv", "polls.csv", mode = "wb")
polls <- read.csv("polls.csv")

polls$date <- format(as.Date(c(paste(polls$year, 
                                     polls$month, 
                                     polls$day,
                                     sep="-")), by = "days"))

for(i in c("a", "b", "c", "f", "i", "k", "o", "v", "oe", "aa")) {
  polls <- within(polls, {
    assign(paste0("ci_", i), 1.96 * sqrt(( get(paste0("party_", i)) * (100 - get(paste0("party_", i)))) / n))
  }
  )
}

polls <- polls[polls$date > as.Date("2015-11-01"),]

polls$megafon <- ifelse(polls$pollingfirm == "Megafon", "blue", "red")

png('megafon.png', height=4, width=6, units="in", res=700)
ggplot(polls, aes(x=as.Date(date), y=party_a)) + 
  geom_ribbon(aes(ymin=party_a-ci_a, ymax=party_a+ci_a), fill="gray", alpha=0.3) +
    geom_point(aes(fill=megafon), colour="black", position="jitter", pch=21, size=5, alpha=0.8) + 
  ylab("Stemmer (%)") +
  xlab("") +
  theme_classic() + theme(legend.position="none")
dev.off()
