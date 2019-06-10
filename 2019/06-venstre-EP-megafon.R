# R script to figures in "Er Venstre gået frem efter EP-valget?"
# Link: https://erikgahner.dk/2019/er-venstre-gaet-frem-efter-ep-valget/

# Load packages
library("RCurl")
library("tidyverse")

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

gh_url <- getURL("https://raw.githubusercontent.com/erikgahner/polls/master/polls.csv")
polls <- read.csv(text = gh_url)

polls$date <- format(as.Date(c(paste(polls$year, 
                                     polls$month, 
                                     polls$day,
                                     sep="-")), by = "days"))


for(i in c("a", "b", "c", "f", "i", "k", "o", "v", "oe", "aa")) {
  polls <- within(polls, {
    assign(paste0("ci_", i), 1.96 * sqrt(( get(paste0("party_", i)) * (100 - get(paste0("party_", i)))) / n))
  }
  )
}

polls <- polls %>%
  filter(date > as.Date("2019-04-01") & date < as.Date("2019-06-01"))

polls_recent <- polls %>%
  filter(date > as.Date("2019-05-19") & date < as.Date("2019-06-01"))

png('venstreEP-megafon.png', height=4, width=7, units='in', res=150)
polls %>%
  filter(pollingfirm == "Megafon") %>%
  ggplot(aes(x=as.Date(date), y=party_v)) + 
  geom_point(size=2) + 
  geom_vline(xintercept = as.Date("2019-05-26"), linetype = "dashed") +
  geom_ribbon(aes(ymin=party_v-ci_v, ymax=party_v+ci_v), alpha=0.2, colour = NA) +
  ylab("Stemmer (%)") +
  xlab("") +
  theme(legend.position="bottom")
dev.off()

png('venstreEP.png', height=4, width=7, units='in', res=150)
ggplot(polls_recent, aes(x=as.Date(date), y=party_v, fill = pollingfirm, colour = pollingfirm)) + 
  geom_point(size=2) + 
  geom_vline(xintercept = as.Date("2019-05-26"), linetype = "dashed") +
  geom_ribbon(aes(ymin=party_v-ci_v, ymax=party_v+ci_v), alpha=0.2, colour = NA) +
  ylab("Stemmer (%)") +
  xlab("") +
  theme(legend.position="bottom")
dev.off()
