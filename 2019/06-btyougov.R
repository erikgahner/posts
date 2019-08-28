# R script to figures in "Folketingsvalget 2019, BT og YouGov"
# Link: https://erikgahner.dk/2019/folketingsvalget-2019-bt-og-yougov/

Sys.setlocale("LC_TIME", "da_DK.UTF-8")

# Load packages
library("RCurl")
library("tidyverse")

# Load theme
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


for(i in c("a", "b", "c", "d", "e", "f", "i", "k", "o", "p", "v", "oe", "aa")) {
  polls <- within(polls, {
    assign(paste0("ci_", i), 1.96 * sqrt(( get(paste0("party_", i)) * (100 - get(paste0("party_", i)))) / n))
  }
  )
}

polls <- polls %>% 
  mutate(
    date = format(as.Date(c(paste(year,month, day, sep="-")), by = "days"))
  ) %>%
  filter(pollingfirm == "YouGov", date > as.Date("2019-04-25"), date < as.Date("2019-06-04"))

# SF

ggplot(polls, aes(x=as.Date(date), y=party_f)) +
  geom_point(size = 2.5, shape = ifelse(polls$date == "2019-05-07" | polls$date == "2019-05-10", 19, 1), colour = "#9C1D2A") +
  geom_ribbon(aes(ymin=party_f-ci_f, ymax=party_f+ci_f), alpha=0.2, fill = "#9C1D2A", colour = NA) +
  labs(y = "Stemmer (%)",
       x = "") +
  annotate("text", x = as.Date("2019-05-14"), y = 10.8, label = '"Ny måling: SF i massiv fremgang \n – fordobler antallet af mandater"', size=4) +
  geom_segment(aes(x = as.Date("2019-05-13"), xend = as.Date("2019-05-08"), y = 10.2,  yend = 9.3),  size = 0.5) +
  annotate("text", x = as.Date("2019-05-08"), y = 5, label = '"Ny måling: Ét rødt parti går tilbage"', size=4) +  
  geom_segment(aes(x = as.Date("2019-05-08"), xend = as.Date("2019-05-10"), y = 5.4,  yend = 6.9),  size = 0.5) 
ggsave("btyougovSF.png", height = 4, width = 7)

ggplot(polls, aes(x=as.Date(date), y=party_o)) +
  geom_point(size = 2.5, shape = ifelse(polls$date == "2019-05-09", 19, 1), colour = "#3D6F8D") +
  geom_ribbon(aes(ymin=party_o-ci_o, ymax=party_o+ci_o), alpha=0.2, fill = "#3D6F8D", colour = NA) +
  labs(y = "Stemmer (%)",
       x = "") +
  annotate("text", x = as.Date("2019-05-09"), y = 8, label = '"Ny måling: Dansk Folkeparti får\nlaveste opbakning i 10 år"', size=4) 
ggsave("btyougovDF.png", height = 4, width = 7)


ggplot(polls, aes(x=as.Date(date), y=party_d)) +
  geom_point(size = 2.5, shape = ifelse(polls$date == "2019-05-15", 19, 1), colour = "#05454F") +
  geom_ribbon(aes(ymin=party_d-ci_d, ymax=party_d+ci_d), alpha=0.2, fill = "#05454F", colour = NA) +
  labs(y = "Stemmer (%)",
       x = "") +
  annotate("text", x = as.Date("2019-05-05"), y = 2.5, label = '"Meningsmåling forværrer\nNye Borgerliges nedtur"', size=4) 
ggsave("btyougovNB.png", height = 4, width = 7)

ggplot(polls, aes(x=as.Date(date), y=party_p)) +
  geom_point(size = 2.5, shape = ifelse(polls$date == "2019-05-21" | polls$date == "2019-05-29", 19, 1), colour = "#000000") +
  geom_ribbon(aes(ymin=party_p-ci_p, ymax=party_p+ci_p), alpha=0.2, fill = "#000000", colour = NA) +
  labs(y = "Stemmer (%)", x = "") +
  annotate("text", x = as.Date("2019-05-13"), y = 1.85, label = '"Stram Kurs nærmer\nsig spærregrænsen"', size=4) 
ggsave("btyougovSK.png", height = 4, width = 7)

ggplot(polls, aes(x=as.Date(date), y=party_c)) +
  geom_point(size = 2.5, shape = ifelse(polls$date == "2019-05-18", 19, 1), colour = "#429969") +
  geom_ribbon(aes(ymin=party_c-ci_c, ymax=party_c+ci_c), alpha=0.2, fill = "#429969", colour = NA) +
  labs(y = "Stemmer (%)", x = "") +
  annotate("text", x = as.Date("2019-05-22"), y = 8, label = '"Regeringen får vælgertæsk\n- men et parti går frem"', size=4) 
ggsave("btyougovC.png", height = 4, width = 7)

ggplot(polls, aes(x=as.Date(date), y=party_v)) +
  geom_point(size = 2.5, shape = ifelse(polls$date == "2019-05-14" | polls$date == "2019-05-16" | polls$date == "2019-05-19", 19, 1), colour = "#459BC8") +
  geom_ribbon(aes(ymin=party_v-ci_v, ymax=party_v+ci_v), alpha=0.2, fill = "#459BC8", colour = NA) +
  labs(y = "Stemmer (%)", x = "") +
  annotate("text", x = as.Date("2019-05-19"), y = 21, label = '"Lille Venstre-spring efter omstridt bog:\nLøkke er i gang med at sminke liget"', size=4) +
  annotate("text", x = as.Date("2019-05-07"), y = 14, label = '"Venstre står til katastrofevalg\ni ny måling: Det er tid\ntil at gå en smule i panik"', size=4) +
  annotate("text", x = as.Date("2019-05-27"), y = 14, label = '"Venstre på katastrofekurs: \nNy måling tyder på\ndårligste valg i 29 år"', size=4) +
  geom_segment(aes(x = as.Date("2019-05-10"), xend = as.Date("2019-05-13"), y = 15.2,  yend = 16.3),  size = 0.5) +
  geom_segment(aes(x = as.Date("2019-05-17"), xend = as.Date("2019-05-20"), y = 15.3,  yend = 14.9),  size = 0.5) +
  geom_segment(aes(x = as.Date("2019-05-17"), xend = as.Date("2019-05-19"), y = 20,  yend = 17),  size = 0.5) 
ggsave("btyougovV.png", height = 4, width = 7)
