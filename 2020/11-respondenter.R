# R script to figures in "Hvorfor er flere respondenter ikke nødvendigvis bedre? #2"
# Link: https://erikgahner.dk/2020/hvorfor-er-flere-respondenter-ikke-nodvendigvis-bedre-2/

# Load packages
library("tidyverse")

Sys.setlocale("LC_TIME", "da_DK.UTF-8")
options(OutDec= ",")

# Theme
theme_set(
  theme_grey(base_size = 11.5) %+replace% 
    theme(
      plot.margin = unit(rep(0.5, 4), "cm"), plot.background = element_blank(), panel.background = element_blank(),
      panel.border = element_blank(), legend.background = element_rect(fill = "transparent", colour = NA),
      legend.key = element_rect(fill = "transparent", colour = NA), legend.title = element_blank(),
      panel.grid.major = element_line(linetype = "dotted", colour = "#757575", size = 0.3), panel.grid.minor = element_blank(),
      axis.ticks = element_blank(), axis.line = element_line(color = "#FFFFFF", size = 0.3),
      plot.title = element_text(size = 12, hjust = 0, margin = margin(b = 15)),
      plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(b = 5)),
      plot.caption = element_text(size = 10, colour = "#212121", margin = margin(t = 15)),
      axis.title = element_text(size = 11, face = "plain"), axis.text = element_text(size = 10, face = "plain"),
      legend.text = element_text(size = 10), strip.text = element_text(size = 12, face = "plain")
    )
)

polls <- read_csv("https://raw.githubusercontent.com/erikgahner/polls/master/polls.csv",
                  col_types = cols(
                    party_e = col_double(),
                    party_g = col_double(),
                    party_p = col_double()
                  ))

polls <- polls %>% 
  mutate(
    date = as.Date(format(as.Date(c(paste(year, month, day, sep="-")), by = "days")))
  ) %>% 
  arrange(date) 

polls %>% 
  filter(date > as.Date("2020-01-01")) %>%
  drop_na(noparty) %>% 
  ggplot(aes(date, noparty, group = pollingfirm, colour = pollingfirm)) +
  geom_point() +
  theme(legend.position="bottom") +
  ggthemes::scale_colour_colorblind() +
  labs(y = "Andel vælgere uden partivalg (%)",
       x = NULL)

ggsave("respondenter.png", width = 6, height = 4)
