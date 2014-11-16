library(XML)
library(ggplot2)
library(reshape2)

source("http://peterhaschke.com/Code/multiplot.R")

url <- "http://en.wikipedia.org/wiki/Opinion_polling_for_the_next_Danish_general_election"

polls <- readHTMLTable(url)

polls <- polls[[1]]
polls <- polls[-1,1:10]
colnames(polls) <- c("Institut", "id","Venstre","Socialdemokraterne","DF","Radikale","SF","Enhedslisten","LA","Konservative")
polls <- na.omit(polls)
polls <- polls[polls[,1]!="Election Results",]
polls$Institut <- gsub("\\[|1|2|3|\\]", "", polls$Institut)
polls$Institut <- gsub("DR", "Epinion", polls$Institut)
polls$id <-  nrow(polls) - as.numeric(rownames(polls)) + 4

polls$Venstre <- as.numeric(levels(polls$Venstre))[polls$Venstre]
polls$Socialdemokraterne <- as.numeric(levels(polls$Socialdemokraterne))[polls$Socialdemokraterne]
polls$DF <- as.numeric(levels(polls$DF))[polls$DF]
polls$Radikale <- as.numeric(levels(polls$Radikale))[polls$Radikale]
polls$SF <- as.numeric(levels(polls$SF))[polls$SF]
polls$Enhedslisten <- as.numeric(levels(polls$Enhedslisten))[polls$Enhedslisten]
polls$LA <- as.numeric(levels(polls$LA))[polls$LA]
polls$Konservative <- as.numeric(levels(polls$Konservative))[polls$Konservative]

p1 <- ggplot(polls, aes(x=id, y=Venstre)) + geom_point(shape=1) + geom_smooth() + theme_bw() + ylab("Stemmer (%)") + ggtitle("Venstre") + theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()) + scale_x_discrete("Meningsmåling efter valget", breaks=c(1), labels=c("Valg"))
p2 <- ggplot(polls, aes(x=id, y=DF)) + geom_point(shape=1) + geom_smooth() + theme_bw() + ylab("Stemmer (%)") + ggtitle("Dansk Folkeparti") + theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()) + scale_x_discrete("Meningsmåling efter valget", breaks=c(1), labels=c("Valg"))

polls2 <- polls
polls2$id <-  as.character(nrow(polls2) - as.numeric(rownames(polls2)) + 4)
polls2 <- melt(polls2[,1:10])
polls2$id <- as.numeric(polls2$id)

ggplot(polls2, aes(x=id, y=value, colour = variable)) + geom_point() + 
  scale_colour_hue(l=50) + geom_smooth() + theme_bw() + ylab("Stemmer (%)") +
  theme(legend.title = element_blank()) + theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_x_continuous("Meningsmåling efter valget", breaks=c(0), labels=c("Valg"))

multiplot(p1, p2, cols=2)