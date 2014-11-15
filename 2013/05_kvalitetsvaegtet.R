# Script to create figure at http://erikgahner.dk/2013/05/02/kvalitetsvaegtede-gennemsnit-af-meningsmalinger-og-statistisk-usikkerhed/

# Load package(s)
library(ggplot2)

# Load data
vaegtet <- read.csv("05_kvalitetsvaegtet.csv")

# Variable with name for legend
vaegtet$Parti <- vaegtet$diff

# Plot
ggplot(vaegtet, aes(x=gallup, y=diff, color=Parti)) +
  geom_point(shape=1, position=position_jitter(width=0.5,height=0.5), aes(colour = factor(parti))) +  
  scale_x_continuous("Partistørrelse") +
  scale_y_continuous("Differens (Vægtet - Gallup)") +
  geom_smooth(method=lm, se=F)