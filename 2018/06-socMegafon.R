# R script to figures in "Er Socialdemokratiet gået tilbage siden folketingsvalget?"
# Link: http://erikgahner.dk/2018/er-socialdemokratiet-gaet-tilbage-siden-folketingsvalget/

# Load packages
library("downloader")
library("ggplot2")
library("ggrepel")

# Theme
theme_web <- function () { # Build on: https://medium.com/@henry.partridge/developing-a-data-visualisation-style-cd24f88fa59
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
}

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

polls.df <- polls[polls$date > as.Date("2018-01-01") & polls$date < as.Date("2018-06-30"),]
polls.df$point <- ""
polls.df$point[polls.df$date == "2018-06-28"] <- "Seneste Megafon"

png('megafon-S.png', height=4, width=7, units='in', res=400)
ggplot(polls.df, aes(x=as.Date(date), y=party_a)) + 
  geom_smooth(se=FALSE, method="loess", colour="gray80") +
  geom_point(aes(colour=pollingfirm), size=2.5) +
  ylab("Stemmer (%)") +
  xlab("") +
  scale_colour_brewer(palette="Paired") +
  geom_text_repel(label=polls.df$point, 
                  box.padding = unit(0.5, 'lines'),
                  point.padding = unit(1.6, 'lines')
  ) +
  theme_web()
dev.off()
