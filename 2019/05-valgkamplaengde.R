# R script til figur i "Hvor lang er valgkampen sammenlignet med tidligere valgkampe?"
# Link: http://erikgahner.dk/2019/hvor-lang-er-valgkampen-sammenlignet-med-tidligere-valgkampe-2/

library("tidyverse")
library("rvest")

url <- c("https://www.ft.dk/da/folkestyret/valg-og-afstemninger/tal-og-fakta-om-valg-og-afstemninger")

valg <- read_html(url)

data_table <- html_nodes(valg, "table")

valg_df <- html_table(data_table[[9]], fill=TRUE)

names(valg_df) <- c("udskrivelse", "afholdelse", "dage")

valg_df <- valg_df %>%
  mutate(aar = as.numeric(str_sub(udskrivelse, -4)))

# Giv alle valgkampe før 2019 værdien 0
valg_df$group <- 0

# Tilføj 2019 informationer 
valg_df <- rbind(valg_df,
                 c("7. maj 2019", "5. juni 2019", 29, 2019, 1))

png('valgkamplaengde.png', height=4, width=8, units='in', res=200)
ggplot(valg_df, aes(x=aar, y=dage)) +
  geom_point(aes(colour=as.factor(group), size=2)) +
  ggtitle("Valgkampens længde, 1953-2019") +
  ylab("Længde (dage)") +
  xlab("") +
  scale_colour_brewer(palette = "Dark2") +
  theme_minimal() + 
  geom_hline(yintercept=29, colour = "black") +
  theme(legend.position="none")
dev.off()