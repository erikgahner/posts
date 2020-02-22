# R script to figures in "Forurener meningsmålingerne den politiske debat?"
# Link: https://erikgahner.dk/2020/forurener-meningsmalingerne-den-politiske-debat/

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

polls_nb <- polls %>%
  filter(date > as.Date("2019-10-01") & date < as.Date("2020-02-19"))

polls_nb %>%
  ggplot(aes(x=as.Date(date), y=party_d, colour = pollingfirm)) + 
  geom_point(size=3) +
  labs(y = "Opbakning til Nye Borgerlige (%)",
       x = NULL) +
  theme(legend.position="bottom") +
  ggthemes::scale_color_colorblind() 

ggsave("nyeborgerlige.png", width = 7, height = 5)

polls_df <- polls %>%
  filter(date > as.Date("2018-09-01") & date < as.Date("2019-06-06"))

ggplot(polls_df, aes(x=as.Date(date), y=party_o, fill = pollingfirm, colour = pollingfirm)) + 
  geom_point(size=2) + 
  geom_errorbar(aes(ymin = party_o - ci_o, ymax = party_o + ci_o), alpha = 0.5, size = 1) +
  ggthemes::scale_color_colorblind() +
  labs(y = "Opbakning til Dansk Folkeparti (%)",
       x = NULL) +
  theme(legend.position="bottom")

ggsave("danskfolkeparti.png", width = 7, height = 5)
