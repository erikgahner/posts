# R script to produce figures in "Hvor mange vil stemme på Klaus Riskær Pedersen? #2"
# Link: http://erikgahner.dk/2019/hvor-mange-vil-stemme-pa-klaus-riskaer-pedersen-2/

# Load packages
library("RCurl")
library("tidyverse")
library("ggrepel")

# Theme
theme_set(
  theme_grey(base_size = 11.5) %+replace% 
    theme(
      plot.margin = unit(rep(0.5, 4), "cm"), plot.background = element_blank(), panel.background = element_blank(),
      panel.border = element_blank(), legend.background = element_rect(fill = "transparent", colour = NA),
      legend.key = element_rect(fill = "transparent", colour = NA), legend.title = element_blank(),
      strip.background = element_rect(fill = "transparent", colour = NA),
      panel.grid.major = element_line(linetype = "dotted", colour = "#757575", size = 0.3), panel.grid.minor = element_blank(),
      axis.ticks = element_blank(), axis.line = element_line(color = "#FFFFFF", size = 0.3),
      plot.title = element_text(size = 12, hjust = 0, margin = margin(b = 15)),
      plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(b = 5)),
      plot.caption = element_text(size = 10, colour = "#212121", margin = margin(t = 15)),
      axis.title = element_text(size = 11, face = "plain"), axis.text = element_text(size = 10, face = "plain"),
      legend.text = element_text(size = 10), strip.text = element_text(size = 12, face = "plain")
    )
)

# Download file
gh_url <- getURL("https://raw.githubusercontent.com/erikgahner/polls/master/polls.csv")
polls <- read.csv(text = gh_url)

polls$se_e <- 1.96 * sqrt( (polls$party_e * (100-polls$party_e) ) / polls$n)  

# Create date variable
polls$date <- format(as.Date(c(paste(polls$year, 
                                     polls$month, 
                                     polls$day,
                                     sep="-")), by = "days"))

polls.df <- polls %>%
  filter(!is.na(party_e))

png('klausriskaer-polls.png', height=4, width=7, units="in", res=200)
ggplot(polls.df, aes(x=as.Date(date), y=party_e, colour=pollingfirm)) + 
  geom_hline(yintercept=2, colour="gray40") +
  geom_errorbar(aes(ymin=party_e-se_e, ymax=party_e+se_e), width = 0, size = 1.2, alpha = 0.5) +
  geom_point() + 
  ylab("Stemmer (%)") +
  xlab("") +
  scale_colour_brewer(palette="Paired") +
  theme(legend.direction = "horizontal", legend.position = "bottom")
dev.off()

# Get data from Google Trends: https://trends.google.com/trends/explore?date=2019-02-01%202019-04-03&geo=DK&q=klaus%20risk%C3%A6r%20pedersen
gd <- read.csv("multiTimeline.csv", skip = 1) 
names(gd) <- c("date", "searches")

png('klausriskaer-google.png', height=4, width=8, units="in", res=200)
ggplot(gd, aes(x=as.Date(date), y=searches)) +
  geom_line(size=1, colour="#537D7A") +
  labs(y = "Interesse over tid", 
       x = "") +
  annotate("text", x = as.Date("2019-02-25"), y = 93, label = "Partiet bliver \n opstillingsberettiget")
dev.off()