# R script to figures in "Blev meningsmålingerne valgets taber?"
# Link: http://erikgahner.dk/2015/06/19/blev-meningsmaalingerne-valgets-taber/

# Load packages
library(downloader)
library(ggplot2)

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

# Use polls from the 2015 campaign
polls <- polls[polls$date > as.Date("2015-05-26") & polls$date < as.Date("2015-06-19"),]

# Create blue bloc variable
polls$blaa <- polls$party_c + polls$party_i + polls$party_k + polls$party_o + polls$party_v
polls$blaa_ci <- 1.96 * sqrt(( polls$blaa * (100 - polls$blaa)) / polls$n)

# Plot polls for the blue bloc
png('2015blaa.png', height=6, width=6, units='in', res=200)
ggplot(polls[polls$pollingfirm != "Norstat" & polls$pollingfirm != "YouGov",], aes(x=as.Date(date), y=blaa)) + 
  geom_smooth(colour="blue", se=F, method="loess") +
  geom_point(aes(x=as.Date("2015-06-18"),y=3.4+7.5+.8+21.1+19.5), size=5, shape=1) + 
  geom_point(size=2, colour="blue") + 
  geom_ribbon(aes(ymin=blaa-blaa_ci, ymax=blaa+blaa_ci), fill="blue", alpha=0.2) +
  ggtitle("Blå blok i valgkampen") +
  ylab("Stemmer (%)") +
  xlab("") +
  facet_wrap(~ pollingfirm) +
  theme_minimal()
dev.off()

# Plot polls for DF
png('2015df.png', height=6, width=6, units='in', res=200)
ggplot(polls[polls$pollingfirm != "Norstat" & polls$pollingfirm != "YouGov",], aes(x=as.Date(date), y=party_o)) + 
  geom_smooth(colour="orange", se=F, method="loess") +
  geom_point(aes(x=as.Date("2015-06-18"),y=21.1), size=5, shape=1) + 
  geom_point(size=2, colour="orange") + 
  geom_ribbon(aes(ymin=party_o-ci_o, ymax=party_o+ci_o), fill="orange", alpha=0.2) +
  ggtitle("Dansk Folkeparti i valgkampen 2015") +
  ylab("Stemmer (%)") +
  xlab("") +
  facet_wrap(~ pollingfirm) +
  theme_minimal()
dev.off()

# Plot polls for C
png('2015c.png', height=6, width=6, units='in', res=200)
ggplot(polls[polls$pollingfirm != "Norstat" & polls$pollingfirm != "YouGov",], aes(x=as.Date(date), y=party_c)) + 
  geom_smooth(colour="darkgreen", se=F, method="loess") +
  geom_point(aes(x=as.Date("2015-06-18"),y=3.4), size=5, shape=1) + 
  geom_point(size=2, colour="darkgreen") + 
  geom_ribbon(aes(ymin=party_c-ci_c, ymax=party_c+ci_c), fill="darkgreen", alpha=0.2) +
  ggtitle("Konservative i valgkampen 2015") +
  ylab("Stemmer (%)") +
  xlab("") +
  facet_wrap(~ pollingfirm) +
  theme_minimal()
dev.off()

