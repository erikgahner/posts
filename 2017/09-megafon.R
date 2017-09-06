# R script to figures in "Hvorfor styrtdykkede Dansk Folkeparti i meningsmålingerne?"
# Link: http://erikgahner.dk/2017/hvorfor-styrtdykkede-dansk-folkeparti-i-meningsmalingerne/

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

polls.df <- polls[polls$date > as.Date("2017-04-01") & polls$date < as.Date("2017-09-04"),]

png('megafon-df-1.png', height=4, width=7, units='in', res=400)
ggplot(polls.df, aes(x=as.Date(date), y=party_o)) + 
  geom_smooth(se=FALSE, method="loess", colour="gray80") +
  geom_point(aes(colour=pollingfirm), size=2.5) +
  ylab("Stemmer (%)") +
  xlab("") +
  scale_colour_brewer(palette="Paired") +
  geom_segment(aes(x = as.Date("2017-05-01"), xend = as.Date("2017-06-25"), 
                   y = 15.4, yend = 13.5, 
                   colour = "Megafon"),
               arrow = arrow(length = unit(0.03, "npc"))) +
  geom_segment(aes(x = as.Date("2017-07-03"), xend = as.Date("2017-08-26"), 
                   y = 13.5, yend = 15.05, 
                   colour = "Megafon"),
               arrow = arrow(length = unit(0.03, "npc"))) +
  theme_minimal() + theme(legend.title=element_blank())
dev.off()


polls.2017 <- polls[polls$date > as.Date("2017-01-01") & polls$date < as.Date("2017-09-01"),]

png('megafon-df-2.png', height=4, width=7, units='in', res=400)
ggplot(polls.2017, aes(x=party_o, y=party_d)) + 
  geom_smooth(se=FALSE, colour="gray80", method="loess") + 
  geom_smooth(se=FALSE, colour="gray80", method="lm") + 
  geom_point(size=2.5) +
  ylab("Nye Borgerlige, stemmer (%)") +
  xlab("Dansk Folkeparti, stemmer (%)") +
  theme_minimal()
dev.off()

cor(polls.2017$party_o, polls.2017$party_d)


