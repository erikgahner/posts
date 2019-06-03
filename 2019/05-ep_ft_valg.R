# R script til figur i "Hvordan vil danskerne stemme til Europa-Parlamentsvalget? #2"
# Link: http://erikgahner.dk/2019/hvordan-vil-danskerne-stemme-til-europa-parlamentsvalget-2/

library("tidyverse")
library("grid")
library("gridExtra")

votes <- tibble(
  party = rep(c("Socialdemokraterne", "Venstre", "Dansk Folkeparti", "Enhedslisten", 
            "Radikale Venstre", "SF", "Liberal Alliance", "Konservative", 
            "Alternativet", "Folkebevægelsen mod EU"), 4),
  election = rep(c(rep("FT", 10), rep("EP", 10)),2),
  firm = c(rep("Epinion", 20), rep("Norstat", 20)),
  support = c(28.1, 17.5, 13.9, 8.9, 5.5, 7.0, 3.6, 5.2, 4.3, 0,
              27.4, 19.8, 11.9, 6.4, 8.3, 8.5, 2.9, 4.1, 3.8, 6.9,
              28.2, 17.3, 12.7, 9.2, 6.3, 6.8, 3.7, 4.3, 3.8, 0,
              27.7, 18.1, 13.4, 7.3, 8.6, 7.2, 3.5, 3.9, 3.4, 6.8)
)

votes_fv_e <- votes %>% filter(election == "FT" & firm == "Epinion") %>% rename(support_fv = support) %>% select(-election, -firm)
votes_ep_e <- votes %>% filter(election == "EP" & firm == "Epinion") %>% rename(support_ep = support) %>% select(-election, -firm)
votes_wide_e <- left_join(votes_fv_e, votes_ep_e, by = "party")
votes_wide_e <- votes_wide_e %>% mutate(support_dif = support_ep - support_fv)

votes_fv_n <- votes %>% filter(election == "FT" & firm == "Norstat") %>% rename(support_fv = support) %>% select(-election, -firm)
votes_ep_n <- votes %>% filter(election == "EP" & firm == "Norstat") %>% rename(support_ep = support) %>% select(-election, -firm)
votes_wide_n <- left_join(votes_fv_n, votes_ep_n, by = "party")
votes_wide_n <- votes_wide_n %>% mutate(support_dif = support_ep - support_fv)

cols <- c("Venstre" = "#459BC8", 
          "Socialdemokraterne" = "#E3515D", 
          "Dansk Folkeparti" = "#3D6F8D", 
          "SF" = "#9C1D2A", 
          "Konservative" = "#429969", 
          "Radikale Venstre" = "#EB4295", 
          "Enhedslisten" = "#914A4F", 
          "Liberal Alliance" = "#EE9A5F", 
          "Folkebevægelsen mod EU" = "#05454F", 
          "Alternativet" = "#AEFEAF")

ggplot(votes, aes(x = party, y = support, fill = party)) + 
  geom_col() +
  scale_fill_manual(values = cols) +
  facet_wrap(~ election + firm) +
  coord_flip() +
  labs(y = "",
       x = "") +
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
    legend.text = element_text(size = 10), strip.text = element_text(size = 12, face = "plain"),
    legend.position = "none"
  ) 

ggsave("epopbakning.png", width = 8, height = 5)

fig_epinion <- ggplot(votes_wide_e, aes(x = party, y = support_dif, fill = party)) + 
  geom_col() +
  scale_fill_manual(values = cols) +
  coord_flip() +
  labs(y = "Epinion",
       x = "") +
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
    legend.text = element_text(size = 10), strip.text = element_text(size = 12, face = "plain"),
    legend.position = "none"
  ) 

fig_norstat <- ggplot(votes_wide_n, aes(x = party, y = support_dif, fill = party)) + 
  geom_col() +
  scale_fill_manual(values = cols) +
  coord_flip() +
  labs(y = "Norstat",
       x = "") +
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
    legend.text = element_text(size = 10), strip.text = element_text(size = 12, face = "plain"),
    legend.position = "none"
  ) 


png("epopbakning_dif.png", width = 1000, height = 800, res = 180)
grid.arrange(fig_epinion, fig_norstat, ncol=1)
dev.off()

