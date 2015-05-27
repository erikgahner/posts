# R script til figur i "Hvor lang er valgkampen sammenlignet med tidligere valgkampe?"
# Link: http://erikgahner.dk/2015/05/27/hvor-lang-er-valgkampen-sammenlignet-med-tidligere-valgkampe/

# Åben pakker. Kan installeres med install.packages("")
library(XML)
library(ggplot2)

# Hent data fra Folketingets hjemmeside
valg <- readHTMLTable("http://www.ft.dk/Folketinget/Oplysningen/Valg/Valgkampens_laengde.aspx")
valg <- valg[[1]]
valg <- valg[-1,]

# Lav variabel med år
valg$aar <- sub('^.* ([[:alnum:]]+)$', '\\1', valg$V2)
valg$aar <- as.numeric(valg$aar)

# Lav længde variabel
valg$laengde <- as.numeric(as.character(valg$V3))

# Giv alle valgkampe før 2015 værdien 0
valg$group <- 0

# Tilføj 2015 informationer 
valg <- rbind(valg,c(NA,NA,NA,2015,23,1))

# Lav figur
png('valgkamplaengde.png', height=6, width=6, units='in', res=200)
ggplot(valg, aes(x=aar, y=laengde)) +
  geom_point(aes(colour=as.factor(group), size=2)) +
  geom_smooth(se=F) +
  ggtitle("Valgkampens længde, 1953-2015") +
  ylab("Længde (dage)") +
  scale_x_continuous(breaks=c(1953,1960,1970,1980,1990,2000,2010,2015)) +
  xlab("") +
  expand_limits(y = 0) +
  theme_minimal() + 
  theme(legend.position="none")
dev.off()
