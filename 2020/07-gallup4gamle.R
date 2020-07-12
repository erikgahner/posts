# R script to figure in "De fire gamle partier i meningsmålingerne"
# Link: https://erikgahner.dk/2020/de-fire-gamle-partier-i-meningsmalingerne/

library("tidyverse")
library("ggforce")

# En stor del af koden er fra: https://github.com/Straubinger/dataviz/blob/master/01_lix_pm_dk/lix_pm_dk.R
## Se også: https://straubinger.netlify.app/project/2020-02-23_lix_pm_dk/

# Ikke offentligt tilgængelige Gallup data (fra 1957 til 2014)
gallup_raw <- read_csv("~/Google Drev/data/03-polls/gallup.csv")
gallup <- gallup_raw %>% 
  mutate(date = as.Date(paste0(year, "-", month, "-", day))) %>% 
  select(date, party_a, party_b, party_c, party_v) %>% 
  pivot_longer(starts_with("party_"), names_to = "party", values_to = "support") %>% 
  filter(support < 60)

# Meningsmålingsdata fra 2010 og frem
polls_raw <- read_csv("https://raw.githubusercontent.com/erikgahner/polls/master/polls.csv")
polls <- polls_raw %>% 
  mutate(date = as.Date(paste0(year, "-", month, "-", day))) %>% 
  filter(date > max(gallup$date), pollingfirm == "Gallup") %>% 
  select(date, party_a, party_b, party_c, party_v) %>% 
  pivot_longer(starts_with("party_"), names_to = "party", values_to = "support") %>% 
  bind_rows(gallup)

# Antal meningsmålinger
NROW(filter(polls, party == "party_a"))

# Ældste meningsmåling
min(polls$date)

# Seneste meningsmåling
max(polls$date)

# Settings
annotate_color <- "#606F7B"
annotate_size <- 2.3
pm_size <- 2.3
placering <- 48
farve_a <- "#E3515D"
farve_b <- "#EB4295"
farve_v <- "#002883"
farve_c <- "#00583c"

pm_annotate <- function(statsminister, dato, hvor) { 
  annotate("text", x = as.Date(dato), y = placering + hvor, hjust = "left", color = annotate_color, size = pm_size, label = statsminister) 
}

segment_pm <- tibble(
  x1 = c(as.Date("1955-02-01"), as.Date("1960-02-21"), as.Date("1962-09-03"), as.Date("1968-02-02"), as.Date("1971-10-11"), 
         as.Date("1972-10-05"), as.Date("1973-12-19"), as.Date("1975-02-13"), as.Date("1982-09-10"), as.Date("1993-01-25"), 
         as.Date("2001-11-27"), as.Date("2009-04-05"), as.Date("2011-10-03"), as.Date("2015-06-28"), as.Date("2019-06-27")),
  x2 = c(as.Date("1960-02-21")-30, as.Date("1962-09-03")-30, as.Date("1968-02-02")-30, as.Date("1971-10-11")-30, as.Date("1972-10-05")-30, 
         as.Date("1973-12-19")-30, as.Date("1975-02-13")-30, as.Date("1982-09-10")-30, as.Date("1993-01-25")-30, as.Date("2001-11-27")-30, 
         as.Date("2009-04-05")-30, as.Date("2011-10-03")-30, as.Date("2015-06-28")-30, as.Date("2019-06-27")-30, as.Date("2020-06-13")),
  y1 = rep(placering, 15),
  y2 = rep(placering, 15),
  col = c(farve_a, farve_a, farve_a, farve_b, farve_a, farve_a, farve_v, farve_a, farve_c, farve_a, farve_v, farve_v, farve_a, farve_v, farve_a)
)

ggplot(polls, aes(x = date, y = support, color = party)) +
  geom_point(size = 1, alpha = 0.5) +
  geom_line(size = 0.1, alpha = 0.5) +
  scale_y_continuous(limits = c(0, 52), breaks=seq(0, 52, 10)) +
  scale_x_date(limits = c(as.Date("1954-04-01"), as.Date("2020-07-01")), breaks = "5 years", labels = scales::date_format("%Y")) +
  scale_color_manual(values = c(farve_v, farve_c, farve_a, farve_b, farve_a, farve_b, farve_c, farve_v)) +
  geom_segment(data = segment_pm, aes(x = x1, y = y1, xend = x2, yend = y2, color = col)) +
  geom_curve(aes(x = as.Date("1967-01-01"), y = 48.5, xend = as.Date("1972-03-01"), yend = 48.5), curvature = -0.3, color = annotate_color, size = 0.3) +
  geom_curve(aes(x = as.Date("1973-08-01"), y = 47.5, xend = as.Date("1976-01-01"), yend = 47.5), curvature = 0.4, color = annotate_color, size = 0.3) +
  geom_curve(aes(x = as.Date("2017-09-01"), y = 52, xend = as.Date("2019-08-01"), yend = 49), arrow = arrow(length = unit(0.05, "in")), curvature = -0.4, color = annotate_color, size = 0.3) +
  pm_annotate("H.C. Hansen", "1955-02-01", 1.5) +
  pm_annotate("Kampmann", "1960-02-21", -1.5) +
  pm_annotate("Krag", "1963-06-01", 1.5) +
  pm_annotate("Baunsgaard", "1968-02-02", -1.5) +
  pm_annotate("Hartling", "1973-12-19", 1.5) +
  pm_annotate("Jørgensen", "1976-09-01", -1.5) +
  pm_annotate("Schlüter", "1982-09-10", -1.5) +
  pm_annotate("Nyrup", "1993-01-25", -1.5) +
  pm_annotate("Fogh", "2001-11-27", -1.5) +
  pm_annotate("Løkke", "2009-04-05", -1.5) +
  pm_annotate("Thorning", "2011-10-03", 1.5) +
  pm_annotate("Løkke", "2015-06-28", -1.5) +
  pm_annotate("Frederiksen", "2012-06-01", 4) +
  labs(title ="De fire gamle partier og 63 års Gallup-målinger",
       subtitle = "Socialdemokraterne, Venstre, Det Konservative Folkeparti og Radikale Venstre, 1957-2020") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(colour = annotate_color, margin = margin(t = 10)),
        axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks.x = element_line()) 

ggsave("gallup4gamle.png", width = 8, height = 4)
