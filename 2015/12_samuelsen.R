# R script til: Hvilke meningsmålinger bliver delt?
# http://erikgahner.dk/2015/12/21/hvilke-meningsmaalinger-bliver-delt/

library(downloader)
library(ggplot2)
library(scales)

download("https://raw.githubusercontent.com/erikgahner/polls/master/polls.csv", "polls.csv", mode = "wb")
polls <- read.csv("polls.csv")

polls$date <- format(as.Date(c(paste(polls$year, 
                                     polls$month, 
                                     polls$day,
                                     sep="-")), by = "days"))

polls <- polls[polls$date > as.Date("2015-01-01") & polls$date < as.Date("2015-12-18"),]

polls$samuelsen <- 0

## https://www.facebook.com/AndersSamuelsenLA/photos/a.459540352365.256847.44833802365/10154452112542366/
polls[polls$pollingfirm == "Epinion" & polls$month == 12 & polls$day == 16,]$samuelsen <- 1

## https://www.facebook.com/AndersSamuelsenLA/photos/pb.44833802365.-2207520000.1450704225./10154359854837366/
polls[polls$pollingfirm == "Megafon" & polls$month == 10 & polls$day == 28,]$samuelsen <- 1

## https://www.facebook.com/AndersSamuelsenLA/photos/pb.44833802365.-2207520000.1450704326./10154271588297366/
polls[polls$pollingfirm == "Gallup" & polls$month == 9 & polls$day == 10,]$samuelsen <- 1

## https://www.facebook.com/AndersSamuelsenLA/photos/pb.44833802365.-2207520000.1450704326./10154244249657366/
polls[polls$pollingfirm == "YouGov" & polls$month == 8 & polls$day == 31,]$samuelsen <- 1

## https://www.facebook.com/AndersSamuelsenLA/photos/pb.44833802365.-2207520000.1450704420./10154028979007366/
polls[polls$pollingfirm == "Greens" & polls$month == 6 & polls$day == 10,]$samuelsen <- 1


## https://www.facebook.com/AndersSamuelsenLA/photos/pb.44833802365.-2207520000.1450704491./10154016088357366/
polls[polls$pollingfirm == "Epinion" & polls$month == 6 & polls$day == 3,]$samuelsen <- 1

## https://www.facebook.com/AndersSamuelsenLA/photos/pb.44833802365.-2207520000.1450704491./10154015433622366/
polls[polls$pollingfirm == "Greens" & polls$month == 6 & polls$day == 2,]$samuelsen <- 1

## https://www.facebook.com/AndersSamuelsenLA/photos/pb.44833802365.-2207520000.1450704703./10153990376502366/
polls[polls$pollingfirm == "Megafon" & polls$month == 5 & polls$day == 27,]$samuelsen <- 1

## https://www.facebook.com/AndersSamuelsenLA/photos/pb.44833802365.-2207520000.1450704703./10153983171107366/
polls[polls$pollingfirm == "YouGov" & polls$month == 5 & polls$day == 25,]$samuelsen <- 1

## https://www.facebook.com/AndersSamuelsenLA/photos/pb.44833802365.-2207520000.1450705070./10153939162647366/
polls[polls$pollingfirm == "YouGov" & polls$month == 5 & polls$day == 11,]$samuelsen <- 1

## https://www.facebook.com/AndersSamuelsenLA/photos/pb.44833802365.-2207520000.1450705070./10153939632792366/
polls[polls$pollingfirm == "Wilke" & polls$month == 5 & polls$day == 10,]$samuelsen <- 1

## https://www.facebook.com/AndersSamuelsenLA/photos/pb.44833802365.-2207520000.1450705070./10153933713812366/
polls[polls$pollingfirm == "Voxmeter" & polls$month == 5 & polls$day == 10,]$samuelsen <- 1

## https://www.facebook.com/AndersSamuelsenLA/photos/pb.44833802365.-2207520000.1450707865./10153912951612366/
polls[polls$pollingfirm == "YouGov" & polls$month == 4 & polls$day == 26,]$samuelsen <- 1

## https://www.facebook.com/AndersSamuelsenLA/photos/pb.44833802365.-2207520000.1450707865./10153880546562366/
polls[polls$pollingfirm == "YouGov" & polls$month == 4 & polls$day == 13,]$samuelsen <- 1
polls[polls$pollingfirm == "Wilke" & polls$month == 4 & polls$day == 14,]$samuelsen <- 1

## https://www.facebook.com/AndersSamuelsenLA/photos/pb.44833802365.-2207520000.1450708320./10153839897567366/
polls[polls$pollingfirm == "Greens" & polls$month == 3 & polls$day == 26,]$samuelsen <- 1

## https://www.facebook.com/AndersSamuelsenLA/photos/pb.44833802365.-2207520000.1450708347./10153795400582366/
polls[polls$pollingfirm == "YouGov" & polls$month == 3 & polls$day == 9,]$samuelsen <- 1

## https://www.facebook.com/AndersSamuelsenLA/photos/pb.44833802365.-2207520000.1450708348./10153745770542366/
polls[polls$pollingfirm == "Megafon" & polls$month == 2 & polls$day == 19,]$samuelsen <- 1

png("samuelsen.png", height=5, width=5, units="in",res=700)
ggplot(polls, aes(x=as.Date(date), y=party_i)) + 
  geom_line(stat="smooth", data=polls[polls$samuelsen==0,], size=1, se=F, colour="blue", alpha=0.7) +
  geom_line(stat="smooth", method="loess", data=polls[polls$samuelsen==1,], size=1, se=F, colour="red", alpha=0.7) +
  geom_point(data=polls[polls$samuelsen==1,], size=3, colour="red", alpha=0.5) + 
  geom_point(data=polls[polls$samuelsen==0,], size=3, colour="blue", alpha=0.3) + 
  scale_x_date(breaks = date_breaks("2 months"), labels = date_format("%b")) +
  ylab("Liberal Alliances opbakning (%)") +
  xlab("") +
  theme_minimal()
dev.off()
