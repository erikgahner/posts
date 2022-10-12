# R script for "Hvor lang er valgkampen sammenlignet med tidligere valgkampe? #3"
# URL: https://erikgahner.dk/2022/hvor-lang-er-valgkampen-sammenlignet-med-tidligere-valgkampe-3/

library("tidyverse")
library("rvest")

url <- c("https://www.ft.dk/da/folkestyret/valg-og-afstemninger/tal-og-fakta-om-valg-og-afstemninger")

valg <- read_html(url)

data_table <- html_nodes(valg, "table")

valg_df <- data_table |> 
  pluck(9) |> 
  html_table(fill=TRUE) |> 
  setNames(c("udskrivelse", "afholdelse", "dage")) |> 
  add_row(udskrivelse = "5. oktober 2022",
          afholdelse = "1. november 2022",
          dage = round(parse_number(format(difftime("2022-11-01", "2022-10-05", units = "days"))), 0)
  ) |> 
  mutate(aar = as.numeric(str_sub(udskrivelse, -4))) |> 
  mutate(gruppe = ifelse(aar == 2022, 1, 0))

valg_df |> 
  ggplot(aes(x = aar, y = dage, label = dage)) +
  geom_line(colour = "black") +
  geom_label(label.size = NA, nudge_y = case_when(valg_df$aar == 1987 ~ -2, 
                                                  valg_df$aar == 1981 ~ 1,
                                                  TRUE ~ 0)) +
  labs(caption = "Valgkampens længde i dage, 1953-2022",
       x = NULL,
       y = NULL) +
  theme_bw() + 
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(), 
        panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = valg_df$aar, labels = valg_df$aar,
                     guide = guide_axis(check.overlap	= TRUE)) 

ggsave("valgkamp_laengde.png", width = 8, height = 4, bg = "white")


