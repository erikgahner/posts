# R script to "Hvor stor er Pape-effekten?"
# Link: http://erikgahner.dk/2017/hvor-stor-er-pape-effekten/

# Load packages
library(downloader)
library(ggplot2)
library(ggrepel)

# Download file
download("https://raw.githubusercontent.com/erikgahner/polls/master/polls.csv", "polls.csv", mode = "wb")
polls <- read.csv("polls.csv")

# See top rows
head(polls)

# Create date variable
polls$date <- format(as.Date(c(paste(polls$year, 
                                     polls$month, 
                                     polls$day,
                                     sep="-")), by = "days"))


# Calcuate 95% confidence intervals
for(i in c("a", "b", "c", "f", "i", "k", "o", "v", "oe", "aa")) {
  polls <- within(polls, {
    assign(paste0("ci_", i), 1.96 * sqrt(( get(paste0("party_", i)) * (100 - get(paste0("party_", i)))) / n))
  }
  )
}

polls.1415 <- polls[polls$date > as.Date("2014-07-01") & polls$date < as.Date("2015-05-01"),]
polls.recent <- polls[polls$date > as.Date("2017-01-01") & polls$date < as.Date("2017-06-01"),]
polls.recent$point <- ""
polls.recent$point[polls.recent$date == "2017-04-27"] <- "Megafon"

png('pape-1.png', height=4, width=6, units='in', res=200)
ggplot(polls.1415, aes(x=as.Date(date), y=party_c)) + 
  geom_smooth(colour="darkgreen", se=F, method="loess") +
  geom_vline(xintercept = as.numeric(as.Date("2014-08-01")),  alpha=0.5, linetype = "longdash") +
  geom_point(size=2, colour="darkgreen", alpha=0.5) + 
  ylab("Stemmer (%)") +
  xlab("") +
  theme_minimal()
dev.off()

png('pape-2.png', height=4, width=6, units='in', res=200)
ggplot(polls.recent, aes(x=as.Date(date), y=party_c)) + 
  geom_smooth(method="loess", colour="darkgreen", se=F) +
  geom_point(size=2, colour="darkgreen", alpha=0.5) + 
  ylab("Stemmer (%)") +
  xlab("") +
  geom_text_repel(label=polls.recent$point, 
                  box.padding = unit(0.5, 'lines'),
                  point.padding = unit(1.6, 'lines')
                  ) +
  theme_minimal()
dev.off()
