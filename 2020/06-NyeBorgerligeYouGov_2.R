# R script to figures in "Hvor mange vil stemme på Nye Borgerlige? #9"
# Link: https://erikgahner.dk/2020/hvor-mange-vil-stemme-pa-nye-borgerlige-9/

# Load packages
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

polls <- read_csv("https://raw.githubusercontent.com/erikgahner/polls/master/polls.csv")

polls$date <- format(as.Date(c(paste(polls$year, 
                                     polls$month, 
                                     polls$day,
                                     sep="-")), by = "days"))

for(i in c("d")) {
  polls <- within(polls, {
    assign(paste0("ci_", i), 1.96 * sqrt(( get(paste0("party_", i)) * (100 - get(paste0("party_", i)))) / n))
  }
  )
}

polls_nb <- polls %>%
  filter(date > as.Date("2019-12-31") & date < as.Date("2020-06-28"))

polls_nb %>%
  ggplot(aes(x=as.Date(date), y=party_d, 
             ymin = party_d - ci_d, ymax = party_d + ci_d,
             colour = pollingfirm,
             fill = pollingfirm)) + 
  geom_point(size = 3) +
  geom_ribbon(alpha = .3, colour = "white") +
  labs(y = "Opbakning til Nye Borgerlige (%)",
       x = NULL) +
  theme(legend.position="bottom") +
  ggthemes::scale_color_tableau() +
  ggthemes::scale_fill_tableau()

ggsave("nyeborgerligeYouGov_2.png", width = 8, height = 5)