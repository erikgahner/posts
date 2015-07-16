# R script til figurer i "Vi ved ikke hvad der gik galt med analyseinstitutternes vurdering af Dansk Folkepartis vælgeropbakning"
# Link: http://erikgahner.dk/2015/07/16/vi-ved-ikke-hvad-der-gik-galt-med-analyseinstitutternes-vurdering-af-dansk-folkepartis-vaelgeropbakning/

# Åbn pakker
library(downloader)
library(ggplot2)
library(changepoint)

# Download fil
download("https://raw.githubusercontent.com/erikgahner/polls/master/polls.csv", "polls.csv", mode = "wb")
polls <- read.csv("polls.csv")

# Lav datovariabel
polls$date <- format(as.Date(c(paste(polls$year, 
                                     polls$month, 
                                     polls$day,
                                     sep="-")), by = "days"))

# Fjern meningsmålinger fra før 15. september, 2011
polls <- polls[polls$date > as.Date("2011-09-15"),]

# Udregn 95% konfidensintervaller
polls$ci_o <- 1.96 * sqrt( (polls$party_o * (100-polls$party_o) ) / polls$n)  

# Lav id variabel
polls <- cbind(1:NROW(polls), polls) 
colnames(polls)[1] <- "id2"

# Lav Institut-variabel (nem måde at ændre legend title)
polls$Institut <- polls$pollingfirm

# Find changepoints
point.o = cpt.mean(polls$party_o, method="SegNeigh")
val.o <- cpts(point.o)

polls$vaerdi.o <- length(val.o)+1

for (i in rev(unique(val.o))) { 
  polls$vaerdi.o[polls$id2 <= i] <- length(val.o)+1 - length(unique(polls$vaerdi.o))
}

# Lav figur med Dansk Folkepartis opbakning, 2011-2015
p.o <- ggplot(polls[polls$date < as.Date("2015-06-19"),], aes(x=as.Date(date), y=party_o)) + 
  scale_y_continuous(limits = c(7, 26)) +
  annotate("rect", xmin=as.Date("2015-05-26"), xmax=as.Date("2015-06-19"), ymin=7, ymax=Inf, alpha=0.1, fill="blue") +
  geom_point(size=1.5, aes(colour=Institut)) + 
  geom_ribbon(aes(ymin=party_o-ci_o, ymax=party_o+ci_o), fill="orange", alpha=0.4) + 
  geom_hline(aes(yintercept=21.1), linetype="dashed") +
  ylab("Stemmer (%)") +
  xlab("") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank())

for (i in 1:length(unique(polls$vaerdi.o))) {
  p.o <- p.o + geom_segment(x=as.numeric(as.Date(min(polls$date[polls$vaerdi.o==i]))),
                            xend=as.numeric(as.Date(max(polls$date[polls$vaerdi.o==i]))),
                            y=mean(polls$party_o[polls$vaerdi.o==i]),
                            yend=mean(polls$party_o[polls$vaerdi.o==i]), colour="gray75") 
}

# Gem figur som png
png('df20112015.png', height=4, width=6, units='in', res=800)
p.o
dev.off()

# Gem figur som png med Dansk Folkepartis opbakning i 2015
png('df2015.png', height=4, width=6, units='in', res=300)
ggplot(polls[polls$date > as.Date("2015-01-01"),], aes(x=as.Date(date), y=party_o)) + 
  geom_point(size=1.5, colour="orange") + 
  geom_ribbon(aes(ymin=party_o-ci_o, ymax=party_o+ci_o), fill="orange", alpha=0.2) + 
  geom_hline(aes(yintercept=21.1), linetype="dashed") +
  geom_vline(aes(xintercept=as.numeric(as.Date("2015-05-26"))), colour="gray25", linetype="dotted") +
  geom_vline(aes(xintercept=as.numeric(as.Date("2015-06-18"))), colour="gray25", linetype="dotted") +
  ylab("Stemmer (%)") +
  xlab("") +
  facet_wrap(~ pollingfirm) +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank())
dev.off()
