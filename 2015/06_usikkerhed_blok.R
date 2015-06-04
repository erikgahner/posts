# R script til figur i "Den gode, den onde og den grusomme: Mediernes formidling af statistisk usikkerhed i meningsmålingerne"
# Link: http://erikgahner.dk/2015/06/04/den-gode-den-onde-og-den-grusomme-mediernes-formidling-af-statistisk-usikkerhed-i-meningsmaalingerne/

library(ggplot2)

x <- c(1:50)
usikkerhed <- function(x) 1.96 * sqrt((x * (100-x)) / 1000)

usikkerhed.data <- data.frame(x, y = usikkerhed(x))

png('usikkerhed_parti_blok.png', height=5, width=5, units='in', res=200)
ggplot(usikkerhed.data, aes(x,y)) + 
  geom_smooth(se=F) +
  ylab("Statistisk usikkerhed (%), N=1000") +
  xlab("Vælgeropbakning (%)") +
  ggtitle("Maksimal usikkerhed ved parti og blok, illustreret") +
  geom_point(aes(x=25,y=usikkerhed(25))) +
  geom_text(size=5, x=25,y=usikkerhed(25), hjust=0.5, vjust=1.5, label=c("Parti\n(25%)")) +
  geom_point(aes(x=50,y=usikkerhed(50))) +
  geom_text(size=5, x=50,y=usikkerhed(50), hjust=0.7, vjust=1.5, label=c("Blok\n(50%)")) +
  geom_segment(size=1, linetype="dashed", aes(x=0, xend=25, y=usikkerhed(25), yend=usikkerhed(25))) +
  geom_segment(size=1, linetype="dashed", aes(x=0, xend=50, y=usikkerhed(50), yend=usikkerhed(50))) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank())
dev.off()
