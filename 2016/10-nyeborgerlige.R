# R script to figures in "Hvor mange vil stemme på Nye Borgerlige? #3"
# Link: http://erikgahner.dk/2016/hvor-mange-vil-stemme-paa-nye-borgerlige-3/

# Load packages
library("downloader")
library("ggplot2")

# Download file
download("https://raw.githubusercontent.com/erikgahner/polls/master/polls.csv", "polls.csv", mode = "wb")
polls <- read.csv("polls.csv")

# Create date variable
polls$date <- format(as.Date(c(paste(polls$year, 
                                     polls$month, 
                                     polls$day,
                                     sep="-")), by = "days"))


# Calcuate 95% confidence intervals
for(i in c("a", "b", "c", "d", "f", "i", "k", "o", "v", "oe", "aa")) {
  polls <- within(polls, {
    assign(paste0("ci_", i), 1.96 * sqrt(( get(paste0("party_", i)) * (100 - get(paste0("party_", i)))) / n))
  }
  )
}

polls <- polls[polls$date > as.Date("2016-09-21") & polls$date < as.Date("2016-10-31"),]

png('nyeborgerlige.png', height=4, width=7, units='in', res=400)
ggplot(polls, aes(x=as.Date(date), y=party_d)) + 
  geom_hline(aes(yintercept=2), linetype="dashed", colour="gray") +
  geom_hline(aes(yintercept=0)) +
  geom_point(size=2, colour="#01505D") + 
  geom_ribbon(aes(ymin=party_d-ci_d, ymax=party_d+ci_d), fill="#01505D", alpha=0.2) +
  ylab("Stemmer (%)") +
  xlab("") +
  xlim(as.Date("2016-09-21"), as.Date("2016-10-31")) +
  theme_minimal()
dev.off()
