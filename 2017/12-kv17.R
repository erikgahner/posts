# R script to figures in "Smittede kommunalvalget af på partiernes opbakning?"
# Link: http://erikgahner.dk/2017/smittede-kommunalvalget-af-pa-partiernes-opbakning/

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


theme_polls <- function () { # Build on: https://medium.com/@henry.partridge/developing-a-data-visualisation-style-cd24f88fa59
  theme_grey(base_size = 11.5) %+replace% 
    theme(
      plot.margin = unit(rep(0.5, 4), "cm"),
      plot.background = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.key = element_rect(fill = "transparent", colour = NA),
      legend.title = element_blank(),
      strip.background = element_rect(fill = "transparent", colour = NA),
      panel.grid.major = element_line(linetype = "dotted", colour = "#757575", size = 0.3),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_line(color = "#FFFFFF", size = 0.3),
      plot.title = element_text(size = 14, hjust = 0, margin = margin(b = 15)),
      plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(b = 5)),
      plot.caption = element_text(size = 10, colour = "#212121", hjust = 1, margin = margin(t = 15)),
      axis.title = element_text(size = 11, face = "plain", hjust = 1),
      axis.text = element_text(size = 10, face = "plain"),
      legend.text = element_text(size = 10),
      strip.text = element_text(size = 12, face = "plain")
    )
}

polls.df <- polls[(polls$date == as.Date("2017-10-03") & polls$pollingfirm == "Greens") |
                  (polls$date == as.Date("2017-11-30") & polls$pollingfirm == "Greens") |
                  (polls$date == as.Date("2017-10-24") & polls$pollingfirm == "Epinion") |
                  (polls$date == as.Date("2017-12-01") & polls$pollingfirm == "Epinion") |
                  (polls$date == as.Date("2017-11-09") & polls$pollingfirm == "Gallup") |
                  (polls$date == as.Date("2017-12-07") & polls$pollingfirm == "Gallup") |
                  (polls$date == as.Date("2017-11-19") & polls$pollingfirm == "Voxmeter") |
                  (polls$date == as.Date("2017-11-26") & polls$pollingfirm == "Voxmeter") |
                  (polls$date == as.Date("2017-10-26") & polls$pollingfirm == "Megafon") |
                  (polls$date == as.Date("2017-11-30") & polls$pollingfirm == "Megafon")
                    ,]

polls.df$kv <- ifelse(polls.df$date < as.Date("2017-11-21"), 0, 1)
polls.df
polls.dif <- data.frame(firm = rep(unique(polls.df$pollingfirm), 8), 
                       party = c(rep("Socialdemokraterne", 5), 
                                 rep("Radikale Venstre", 5),
                                 rep("Konservative", 5),
                                 rep("Socialistisk Folkeparti", 5),
                                 rep("Liberal Alliance", 5),
                                 rep("Dansk Folkeparti", 5),
                                 rep("Venstre", 5),
                                 rep("Enhedslisten", 5)
                                 ), 
                       dif = NA)
names(polls.df)
for (i in unique(polls.df$pollingfirm)) {
  polls.dif$dif[polls.dif$party == "Socialdemokraterne" & polls.dif$firm == i] <- polls.df$party_a[polls.df$pollingfirm == i & polls.df$kv == 1] - polls.df$party_a[polls.df$pollingfirm == i & polls.df$kv == 0]
  polls.dif$dif[polls.dif$party == "Radikale Venstre" & polls.dif$firm == i] <- polls.df$party_b[polls.df$pollingfirm == i & polls.df$kv == 1] - polls.df$party_b[polls.df$pollingfirm == i & polls.df$kv == 0]
  polls.dif$dif[polls.dif$party == "Konservative" & polls.dif$firm == i] <- polls.df$party_c[polls.df$pollingfirm == i & polls.df$kv == 1] - polls.df$party_c[polls.df$pollingfirm == i & polls.df$kv == 0]
  polls.dif$dif[polls.dif$party == "Socialistisk Folkeparti" & polls.dif$firm == i] <- polls.df$party_f[polls.df$pollingfirm == i & polls.df$kv == 1] - polls.df$party_f[polls.df$pollingfirm == i & polls.df$kv == 0]
  polls.dif$dif[polls.dif$party == "Liberal Alliance" & polls.dif$firm == i] <- polls.df$party_i[polls.df$pollingfirm == i & polls.df$kv == 1] - polls.df$party_i[polls.df$pollingfirm == i & polls.df$kv == 0]
  polls.dif$dif[polls.dif$party == "Dansk Folkeparti" & polls.dif$firm == i] <- polls.df$party_o[polls.df$pollingfirm == i & polls.df$kv == 1] - polls.df$party_o[polls.df$pollingfirm == i & polls.df$kv == 0]
  polls.dif$dif[polls.dif$party == "Venstre" & polls.dif$firm == i] <- polls.df$party_v[polls.df$pollingfirm == i & polls.df$kv == 1] - polls.df$party_v[polls.df$pollingfirm == i & polls.df$kv == 0]
  polls.dif$dif[polls.dif$party == "Enhedslisten" & polls.dif$firm == i] <- polls.df$party_oe[polls.df$pollingfirm == i & polls.df$kv == 1] - polls.df$party_oe[polls.df$pollingfirm == i & polls.df$kv == 0]
}

polls.dif$kv17 <- NA
polls.dif$kv17[polls.dif$party == "Socialdemokraterne"] <- 2.9
polls.dif$kv17[polls.dif$party == "Radikale Venstre"] <- -0.2
polls.dif$kv17[polls.dif$party == "Konservative"] <- 0.2
polls.dif$kv17[polls.dif$party == "Socialistisk Folkeparti"] <- 0.1
polls.dif$kv17[polls.dif$party == "Liberal Alliance"] <- -0.3
polls.dif$kv17[polls.dif$party == "Dansk Folkeparti"] <- -1.3
polls.dif$kv17[polls.dif$party == "Venstre"] <- -3.5
polls.dif$kv17[polls.dif$party == "Enhedslisten"] <- -0.9


png("kv17dif.png", height=4, width=7, units='in', res=250)
ggplot(polls.dif, aes(x=kv17, y=dif, colour=party, shape=firm)) + 
  geom_hline(yintercept=0, size=0.2) +
  geom_point(size=2) +
  theme_polls() +
  ylab("Forskel mellem måling før og efter KV17") +
  xlab("Forskel mellem KV13 og KV17") +
  scale_shape_manual(values = c(4,17,1,16,0)) +
  scale_colour_brewer(palette="Paired") 
dev.off()
