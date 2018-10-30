# R script to produce figure in "Er de Radikale stormet frem?"
# Link: http://erikgahner.dk/2018/er-de-radikale-stormet-frem/

# Load packages
library("downloader")
library("ggplot2")
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
download("https://raw.githubusercontent.com/erikgahner/polls/master/polls.csv", "polls.csv", mode = "wb")
polls <- read.csv("polls.csv")

# Create date variable
polls$date <- format(as.Date(c(paste(polls$year, 
                                     polls$month, 
                                     polls$day,
                                     sep="-")), by = "days"))


polls.df <- polls[polls$date > as.Date("2015-06-15") & polls$date < as.Date("2018-10-30"),]

png('megafon-radikale.png', height=4, width=7, units='in', res=400)
ggplot(polls.df, aes(x=as.Date(date), y=party_b)) + 
  geom_point(aes(colour=pollingfirm), size=1.5) +
  ylab("Stemmer (%)") +
  xlab("") +
  scale_colour_brewer(palette="Paired") +
  geom_text_repel(
    aes(label = ifelse(date == "2018-10-25", "Seneste\n Megafon", NA)),
    size = 4.5,
    point.padding = .2,
    box.padding = .4
  )
dev.off()