# R script to figures in "Hvor mange vil stemme på Nye Borgerlige? #4"
# Link: http://erikgahner.dk/2016/hvor-mange-vil-stemme-paa-nye-borgerlige-4/

library("downloader")
library("ggplot2")
library("RColorBrewer")

download("https://raw.githubusercontent.com/erikgahner/polls/master/polls.csv", "polls.csv", mode = "wb")
polls <- read.csv("polls.csv")

polls$date <- format(as.Date(c(paste(polls$year, 
                                     polls$month, 
                                     polls$day,
                                     sep="-")), by = "days"))


polls <- polls[polls$date > as.Date("2016-09-21") & polls$date < as.Date("2016-11-10"),]

png('nbfirms.png', height=4, width=7, units='in', res=400)
ggplot(polls, aes(x=as.Date(date), y=party_d)) + 
  geom_hline(aes(yintercept=2), linetype="dashed", colour="gray") +
  geom_hline(aes(yintercept=0)) +
  geom_line() +
  geom_point(size = 6, colour = "white") + 
  geom_point(size = 3, shape = 1) + 
  ylab("Stemmer (%)") +
  xlab("") +
  xlim(as.Date("2016-09-21"), as.Date("2016-11-07")) +
  theme_minimal()
dev.off()

png('nbfirms2.png', height=4, width=7, units='in', res=400)
ggplot(polls, aes(x=as.Date(date), y=party_d, , colour=pollingfirm)) + 
  geom_hline(aes(yintercept=2), linetype="dashed", colour="gray") +
  geom_hline(aes(yintercept=0)) +
  geom_point(size=2) + 
  geom_line(size=1) + 
  ylab("Stemmer (%)") +
  xlab("") +
  xlim(as.Date("2016-09-21"), as.Date("2016-11-07")) +
  theme_minimal() +
  scale_colour_manual(values=brewer.pal(8,"Set2")) +
  theme(legend.title=element_blank())
dev.off()