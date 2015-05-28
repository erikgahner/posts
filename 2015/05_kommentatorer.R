# R script til figur i "Hvornår kommer folketingsvalget? De politiske kommentatorers bud #2"
# Link: http://erikgahner.dk/2015/05/28/hvornaar-kommer-folketingsvalget-de-politiske-kommentatorers-bud-2/

# Åben pakker. Kan installeres med install.packages("")
library(ggplot2)

# Lav data frame med kommentatorer og deres gæt
kommentatorer <- data.frame( 
  person = c("David Trads",
             "Søs Marie Serup",
             "Jarl Cordua",
             "Jesper Termansen",
             "Christine Cordsen",
             "Anders Langballe",
             "Hans Engell",
             "Casper Dall",
             "Erik Holsten",
             "Troels Mylenberg",
             "Helle Ib",
             "Peter Mogensen"
    ),
  gaet = c(
    as.Date("2015-04-29"),
    as.Date("2015-05-05"),
    as.Date("2015-05-12"),
    as.Date("2015-05-19"),
    as.Date("2015-05-28"),
    as.Date("2015-06-09"),
    as.Date("2015-06-09"),
    as.Date("2015-08-25"),
    as.Date("2015-09-01"),
    as.Date("2015-09-08"),
    as.Date("2015-09-10"),
    as.Date("2015-09-15")
    )
  )

# Lav variabel med forskellen mellem gættet og valgdatoen
kommentatorer$forskel <- kommentatorer$gaet - as.Date("2015-06-14")

# Lav variabel med den numeriske forskel (altså ingen negative værdier)
kommentatorer$forskel_n <- kommentatorer$forskel
kommentatorer[kommentatorer$forskel_n < 0,]$forskel_n <- kommentatorer[kommentatorer$forskel_n < 0,]$forskel_n * -1

# Lav figur
png('kommentatorer.png', height=6, width=6, units='in', res=200)
ggplot(kommentatorer, aes(x = gaet, y = reorder(person, -forskel ))) +
  geom_point() +
  annotate("rect", xmin=as.Date("2015-06-14") - sd(kommentatorer$forskel_n), xmax=as.Date("2015-06-14") + sd(kommentatorer$forskel_n), ymin=0, ymax=Inf, alpha=0.1, fill="red") +
  geom_text(aes(label=forskel_n), hjust=0.5, vjust=1.5) +
  ggtitle("De politiske kommentatorers bud på en valgdato") +
  xlab("") +
  ylab("") + 
  geom_vline(xintercept = as.numeric(as.Date("2015-06-14")), colour="red", linetype = "longdash") +
  theme_minimal()
dev.off()

