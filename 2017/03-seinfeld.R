# R script to "Seinfeld"
# Link: http://erikgahner.dk/2017/seinfeld/

library("ggplot2")
library("dplyr")

knud <- read.csv("imdb-knud.csv")
erik <- read.csv("imdb-erik.csv")

k.imdb <- knud[grepl("Seinfeld: ", knud$Title),]
k.imdb$person <- "Knud"
names(k.imdb)[9] <- "rating"
names(k.imdb)[15] <- "dato"

e.imdb <- erik[grepl("Seinfeld: ", erik$Title),]
e.imdb$person <- "Erik"
names(e.imdb)[9] <- "rating"
names(e.imdb)[15] <- "dato"

seinfeld <- rbind(select(k.imdb, Title, person, rating, dato), select(e.imdb, Title, person, rating, dato))
seinfeld$rating <- as.numeric(seinfeld$rating)

table(filter(seinfeld, person == "Erik")$rating)
table(filter(seinfeld, person == "Knud")$rating)

summary(filter(seinfeld, person == "Knud")$rating)
summary(filter(seinfeld, person == "Erik")$rating)

sd(filter(seinfeld, person == "Knud")$rating)
sd(filter(seinfeld, person == "Erik")$rating)

png('seinfeld-hist.png', height=5, width=6, units='in', res=500)
ggplot(seinfeld, aes(x=rating, fill=person)) +
  geom_histogram(binwidth=.5, position="dodge") +
  scale_fill_manual(values=c("#E30922", "#FCD720")) + 
  theme_minimal() +
  scale_x_continuous("", limits=c(1,10.3), breaks=1:10) +
  ylab("") +
  theme(legend.title=element_blank(), legend.position="bottom") 
dev.off()

imdb <- erik[grepl("Seinfeld: ", erik$Title),]
imdb$person <- "IMDb"
names(imdb)[10] <- "rating"
names(imdb)[15] <- "dato"

seinfeld <- rbind(seinfeld, select(imdb, Title, person, rating, dato))
seinfeld$rating <- as.numeric(seinfeld$rating)

png('seinfeld-tid.png', height=5, width=6, units='in', res=500)
ggplot(seinfeld, aes(x=as.Date(dato), y=rating, colour=person)) +
  geom_jitter(alpha=0.3, height = 0.2) +
  geom_smooth(se=FALSE) +
  scale_colour_manual(values= c("#E30922", "#000000", "#FCD720")) + 
  theme_minimal() +
  xlab("") +
  scale_y_continuous("Vurdering", limits=c(1,10.5), breaks=1:10) +
  theme(legend.title=element_blank(), legend.position="bottom") 
dev.off()
