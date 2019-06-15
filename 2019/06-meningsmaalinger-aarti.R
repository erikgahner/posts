# R script to figures in "Partierne i meningsmålingerne fra 2010 til i dag"
# Link: https://erikgahner.dk/2019/partierne-i-meningsmalingerne-fra-2010-til-i-dag/

# Load packages
library("RCurl")
library("tidyverse")

gh_url <- getURL("https://raw.githubusercontent.com/erikgahner/polls/master/polls.csv")
polls <- read.csv(text = gh_url)

polls <- polls %>% 
  mutate(
    date = format(as.Date(c(paste(year,month, day, sep="-")), by = "days"))
  )

for(i in c("a", "b", "c", "d", "e", "f", "i", "k", "o", "p", "v", "oe", "aa")) {
  polls <- within(polls, {
    assign(paste0("ci_", i), 1.96 * sqrt(( get(paste0("party_", i)) * (100 - get(paste0("party_", i)))) / n))
  }
  )
}

# Plot for blog post

polls %>%
  gather(party, support, party_a, party_v) %>%
  ggplot(aes(x=as.Date(date), y=support, colour=party)) +
  geom_point(size=1, alpha=0.3) +
  labs(y = "Stemmer (%)",
       x = "") +
  scale_colour_manual(labels = c("Socialdemokraterne", "Venstre"), 
                      values = c("#E3515D", "#459BC8")) +
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
    legend.position = "none",
    legend.text = element_text(size = 10), strip.text = element_text(size = 12, face = "plain")
  ) +
  scale_y_continuous(sec.axis = sec_axis(~ ., breaks = c(17.8, 26.8), labels = c("Venstre", "Socialdemokraterne"))) 
ggsave("support-SV.png", height = 4, width = 8)

max(polls$party_v, na.rm=TRUE)

# Plots for TV 2

plot_party <- function(x, y){
  ggplot(polls, aes_string(x="as.Date(date)", y=x)) + 
    geom_point(aes(colour=pollingfirm, shape=pollingfirm), size=2.5, alpha = 0.8) +
    ylab("Stemmer (%)") +
    xlab("") +
    ggtitle(y) +
    scale_colour_brewer(palette="Paired") +
    scale_shape_manual(values = c(16,0,18,2,3,4,17,5, 1)) +
    {if(with(polls, min(get(x), na.rm=TRUE)) < 4) geom_hline(yintercept=2, linetype = "dashed") }+
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

png('support-a.png', width = 1200, height = 650, units = "px", res = 180)
plot_party("party_a", "Socialdemokratiet")
dev.off()

png('support-b.png', width = 1200, height = 650, units = "px", res = 180)
plot_party("party_b", "Radikale Venstre")
dev.off()

png('support-c.png', width = 1200, height = 650, units = "px", res = 180)
plot_party("party_c", "Konservative")
dev.off()

png('support-d.png', width = 1200, height = 650, units = "px", res = 180)
plot_party("party_d", "Nye Borgerlige")
dev.off()

png('support-f.png', width = 1200, height = 650, units = "px", res = 180)
plot_party("party_f", "SF")
dev.off()

png('support-i.png', width = 1200, height = 650, units = "px", res = 180)
plot_party("party_i", "Liberal Alliance")
dev.off()

png('support-o.png', width = 1200, height = 650, units = "px", res = 180)
plot_party("party_o", "Dansk Folkeparti")
dev.off()

png('support-v.png', width = 1200, height = 650, units = "px", res = 180)
plot_party("party_v", "Venstre")
dev.off()

png('support-oe.png', width = 1200, height = 650, units = "px", res = 180)
plot_party("party_oe", "Enhedslisten")
dev.off()

png('support-aa.png', width = 1200, height = 650, units = "px", res = 180)
plot_party("party_aa", "Alternativet")
dev.off()