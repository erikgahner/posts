# R script til figur i "Hvorfor er stikprøvestørrelsen mindre i den seneste Megafon-måling?"
# Link: http://erikgahner.dk/2015/05/27/hvorfor-er-stikproevestoerrelsen-mindre-i-den-seneste-megafon-maaling/

# Åben pakker. Kan installeres med install.packages("")
library(downloader)
library(ggplot2)

# Download fil fra GitHub
download("https://raw.githubusercontent.com/erikgahner/polls/master/polls.csv", "polls.csv", mode = "wb")

# Hent polls.csv
polls <- read.csv("polls.csv")

# Lav dato variabel
polls$date <- format(as.Date(c(paste(polls$year, polls$month, polls$day, sep="-")), by = "days"))

# Brug kun meningsmålinger fra denne valgperiode
polls <- polls[polls$date > as.Date("2011-09-15") & polls$date < as.Date("2015-05-26"),]

# Få antallet af meningsmålinger fra Megafon i denne valgperiode
NROW(polls$n[polls$pollingfirm == "Megafon"])

# Se målingerne fra Megafon, der har under 1000 respondenter
polls[polls$n < 1000 & polls$pollingfirm == "Megafon",]

# Lav unik værdi for den seneste Megafon-måling
polls$Periode <- "15. september 2011 til 30. april 2015"
polls$Periode[polls$day == 25 & polls$month == 5 & polls$year == 2015] <- "25. maj 2015"

png('megafon.png', height=4, width=6, units='in', res=200)
ggplot(polls[polls$pollingfirm == "Megafon",], aes(x=n, fill=Periode)) +
 geom_histogram(binwidth = 10) +
 ylab("") +
 ggtitle("Stikprøvestørrelsen i Megafon-målingerne, 2011-2015") +
 xlab("Stikprøvestørrelse") +
 theme_minimal() +
 theme(legend.direction = "horizontal", legend.position = "bottom")
dev.off()

# Udregn statistisk usikkerhed med et 95% konfidensniveau
## N = 870
1.96 * sqrt( (48 * (100-48) ) / 870)

## N = gennemsnit i Megafon for denne valgperiode
1.96 * sqrt( (48 * (100-48) ) / mean(polls$n[polls$pollingfirm == "Megafon"]))