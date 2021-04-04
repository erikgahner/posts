# R script for "Hvor lav er opbakningen til Venstre? #2"
# Link: https://erikgahner.dk/2021/hvor-lav-er-opbakningen-til-venstre-2/

library("tidyverse")

Sys.setlocale("LC_TIME", "da_DK.UTF-8")

polls <- read_csv("https://raw.githubusercontent.com/erikgahner/polls/master/polls.csv")

polls$date <- format(as.Date(c(paste(polls$year, 
                                     polls$month, 
                                     polls$day,
                                     sep="-")), by = "days"))

for(i in c("v")) {
  polls <- within(polls, {
    assign(paste0("ci_", i), 1.96 * sqrt(( get(paste0("party_", i)) * (100 - get(paste0("party_", i)))) / n))
  }
  )
}
 
polls_v <- polls %>%
  filter(date > as.Date("2020-01-01"), date < as.Date("2021-04-04"))

polls_v %>%
  ggplot(aes(x=as.Date(date), y=party_v, 
             ymin = party_v - ci_v, ymax = party_v + ci_v,)) + 
  geom_vline(xintercept = as.Date("2020-03-11"), colour = "#FF4136", linetype = "dashed") +
  geom_vline(xintercept = as.Date("2021-01-01"), colour = "#0074D9", linetype = "dashed") +
  geom_vline(xintercept = as.Date("2021-02-04"), colour = "#FF851B", linetype = "dashed") +
  geom_point(size = 3, pch = 21, color = "white", fill = "black") +
  labs(y = "Opbakning til Venstre (%)",
       x = NULL) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  annotate("text", x = as.Date("2020-04-30"), y = 25.5, label = "Corona-nedlukning", colour = "#FF4136") +
  annotate("text", x = as.Date("2020-11-27"), y = 25.5, label = "Lars Løkke", colour = "#0074D9") +
  annotate("text", x = as.Date("2021-03-15"), y = 25.5, label = "Inger Støjberg", colour = "#FF851B") +
  theme_minimal()

ggsave("venstreopbakning.png", width = 8, height = 4)
