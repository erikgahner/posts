# Load package(s)
library(ggplot2)

# Data source: Descriptive statistics from variable v14_1a in the Danish National Election Study, 2011
problem <- data.frame(
	x = c("Økonomi","Velfærdsstaten\ngenerelt", "Arbejdsmarked","Sundhed","Ældre", "Miljø og Energi","Flygtninge"),
	y = c(767,337,291,85,83,60,46),
	emne = c(1,1,1,1,1,1,0)
)

# Reorder variable 
problem$x <- reorder(problem$x, -problem$y)

# Plot
ggplot(data=problem, aes(x=x, y=y, fill=as.factor(emne))) + 
	geom_bar(stat="identity") +
	theme_bw() + 
	xlab("") +
	ylab("") +
	theme(legend.position="none")