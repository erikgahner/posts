# R script to figures in "Blev vi oversvømmet med meningsmålinger i valgkampen?"
# Link: https://erikgahner.dk/2019/blev-vi-oversvommet-med-meningsmalinger-i-valgkampen/

library("RCurl")
library("tidyverse")
library("ggcal") # devtools::install_github("jayjacobs/ggcal")
library("gridExtra")
library("cowplot")

Sys.setlocale("LC_TIME", "da_DK.UTF-8")

gh_url <- getURL("https://raw.githubusercontent.com/erikgahner/polls/master/polls.csv")
polls <- read.csv(text = gh_url)

polls <- polls %>% 
  mutate(
    date = format(as.Date(c(paste(year,month, day, sep="-")), by = "days"))
  )

polls_freq <- polls %>%
  group_by(date) %>%
  summarise(freq = n())
polls_freq$date <- as.Date(polls_freq$date)


# 2011: 20 dage
sum(polls_freq$freq[polls_freq$date > "2011-08-25" & polls_freq$date < "2011-09-16"])
sum(polls_freq$freq[polls_freq$date > "2011-08-25" & polls_freq$date < "2011-09-16"])/20
# 2015: 22 dage 
sum(polls_freq$freq[polls_freq$date > "2015-05-26" & polls_freq$date < "2015-06-19"])
sum(polls_freq$freq[polls_freq$date > "2015-05-26" & polls_freq$date < "2015-06-19"])/22
# 2019: 29 dage
sum(polls_freq$freq[polls_freq$date > "2019-05-06" & polls_freq$date < "2019-06-06"])
sum(polls_freq$freq[polls_freq$date > "2019-05-06" & polls_freq$date < "2019-06-06"])/29

p2011 <- data.frame(date = seq(as.Date("2011-08-01"), as.Date("2011-09-30"), by="1 day"))
p2011 <- left_join(p2011, polls_freq, by = "date")

p2015 <- data.frame(date = seq(as.Date("2015-05-01"), as.Date("2015-06-30"), by="1 day"))
p2015 <- left_join(p2015, polls_freq, by = "date")

p2019 <- data.frame(date = seq(as.Date("2019-05-01"), as.Date("2019-06-30"), by="1 day"))
p2019 <- left_join(p2019, polls_freq, by = "date")

plot_2011 <- ggcal(p2011$date, p2011$freq) + scale_fill_gradient2(limits = c(0,8), low="#4575b4", mid="#ffffbf", high="#d73027", na.value="white") + labs(title = "2011") + theme(legend.position="none")
plot_2015 <- ggcal(p2015$date, p2015$freq) + scale_fill_gradient2(limits = c(0,8), low="#4575b4", mid="#ffffbf", high="#d73027", na.value="white") + labs(title = "2015") + theme(legend.position="none")
plot_2019 <- ggcal(p2019$date, p2019$freq) + scale_fill_gradient2(limits = c(0,8), low="#4575b4", mid="#ffffbf", high="#d73027", na.value="white") + labs(title = "2019") + theme(legend.position="bottom")



png('pollcalendar.png',  width = 750, height = 1300, res = 200)
plot_grid(plot_2011, plot_2015, plot_2019, align = "v", nrow = 3, rel_heights = c(0.295, 0.295, 0.41))
dev.off()
