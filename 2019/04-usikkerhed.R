# R script to produce figure in "Hvorfor skal meningsmålinger være så præcise?"
# Link: http://erikgahner.dk/2019/hvorfor-skal-meningsmalinger-vaere-sa-praecise/

library("tidyverse")
library("gganimate")
library("gifski")
library("RCurl")
library("tidyverse") 
library("magick")

theme_set(
  theme_grey(base_size = 20) %+replace% 
    theme(
      plot.margin = unit(rep(0.5, 4), "cm"), plot.background = element_blank(), panel.background = element_blank(),
      panel.border = element_blank(), legend.background = element_rect(fill = "transparent", colour = NA),
      legend.key = element_rect(fill = "transparent", colour = NA), legend.title = element_blank(),
      strip.background = element_rect(fill = "transparent", colour = NA),
      panel.grid.major = element_line(linetype = "dotted", colour = "#757575", size = 0.3), panel.grid.minor = element_blank(),
      axis.ticks = element_blank(), axis.line = element_line(color = "#FFFFFF", size = 0.3),
      plot.title = element_text(size = 20, hjust = 0, margin = margin(b = 15)),
      plot.subtitle = element_text(size = 20, hjust = 0, margin = margin(b = 5)),
      plot.caption = element_text(size = 20, colour = "#212121", margin = margin(t = 17)),
      axis.title = element_text(size = 20, face = "plain"), axis.text = element_text(size = 20, face = "plain"),
      legend.text = element_text(size = 20), strip.text = element_text(size = 20, face = "plain")
    )
)

# Download file
gh_url <- getURL("https://raw.githubusercontent.com/erikgahner/polls/master/polls.csv")
polls <- read.csv(text = gh_url)

for(i in c("a", "b", "c", "d", "e", "f", "i", "k", "o", "v", "oe", "aa")) {
  polls <- within(polls, {
    assign(paste0("ci90_", i), 1.65 * sqrt(( get(paste0("party_", i)) * (100 - get(paste0("party_", i)))) / n))
    assign(paste0("ci95_", i), 1.96 * sqrt(( get(paste0("party_", i)) * (100 - get(paste0("party_", i)))) / n))
    assign(paste0("ci99_", i), 2.58 * sqrt(( get(paste0("party_", i)) * (100 - get(paste0("party_", i)))) / n))
    
  }
  )
}

# Create date variable
polls$date <- format(as.Date(c(paste(polls$year, 
                                     polls$month, 
                                     polls$day,
                                     sep="-")), by = "days"))


poll <- polls %>%
  filter(as.Date(date) == as.Date("2019-04-07")) %>%
  select(date, starts_with("party_"), starts_with("ci"))

poll_long <- gather(poll, uncertainty, se, ci99_a:ci90_aa, factor_key=TRUE)
poll_long <- gather(poll_long, party, support, party_a:party_aa, factor_key=TRUE)

poll_long <- poll_long %>%
  mutate(Party = case_when(
    party == "party_a" ~ "A",
    party == "party_b" ~ "B",
    party == "party_c" ~ "C",
    party == "party_d" ~ "D",
    party == "party_e" ~ "E",
    party == "party_f" ~ "F",
    party == "party_i" ~ "I",
    party == "party_k" ~ "K",
    party == "party_o" ~ "O",
    party == "party_v" ~ "V",
    party == "party_oe" ~ "Ø",
    party == "party_aa" ~ "Å"
  ))

poll_long <- poll_long %>%
  mutate(support_low = support-se,
         support_high = support+se)

poll_long$support_low <- ifelse(poll_long$support_low < 0, 0, poll_long$support_low)

poll_long <- poll_long %>%
  filter((Party == "A" & endsWith(as.character(uncertainty), "_a")) |
         (Party == "B" & endsWith(as.character(uncertainty), "_b")) |
         (Party == "C" & endsWith(as.character(uncertainty), "_c")) |
         (Party == "D" & endsWith(as.character(uncertainty), "_d")) |
         (Party == "E" & endsWith(as.character(uncertainty), "_e")) |
         (Party == "F" & endsWith(as.character(uncertainty), "_f")) |
         (Party == "I" & endsWith(as.character(uncertainty), "_i")) |
         (Party == "K" & endsWith(as.character(uncertainty), "_k")) |
         (Party == "O" & endsWith(as.character(uncertainty), "_o")) |
         (Party == "V" & endsWith(as.character(uncertainty), "_v")) |
         (Party == "Ø" & endsWith(as.character(uncertainty), "_oe")) |
         (Party == "Å" & endsWith(as.character(uncertainty), "_aa")) 
         ) 

poll_long$period[str_detect(poll_long$uncertainty, "90")] <- 1
poll_long$period[str_detect(poll_long$uncertainty, "95")] <- 2
poll_long$period[str_detect(poll_long$uncertainty, "99")] <- 3

p <- ggplot(poll_long, aes(x=Party, y=support, colour = Party)) + 
  geom_hline(yintercept=2, colour="gray40", linetype = "dashed") +
  geom_hline(yintercept=0, colour="gray40") +
  geom_errorbar(aes(ymin=support_low, ymax=support_high), width = 0, size = 5, alpha = 0.6) +
  geom_point(size=1.5, alpha = 0.6) + 
  labs(title = "Partiernes opbakning, Voxmeter, 7. april 2019",
       y = "Stemmer (%)",
       x = "") +
  scale_colour_manual(values = c("#E3515D", "#AEFEAF", "#EB4295", "#429969",
                                 "#05454F", "#537D7A", "#9C1D2A", "#EE9A5F",
                                 "#F4CE97", "#3D6F8D", "#914A4F", "#459BC8")) +
  theme(legend.position = "none") +
  transition_states(period, transition_length = 0.5, state_length = 0.2)

anim <- animate(p, height = 450, width = 900)
anim_save("polluncertainty.gif", anim)

