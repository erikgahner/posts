# R script til: Er statskundskabsstuderende de mest kæphøje?
# http://erikgahner.dk/2016/02/04/er-statskundskabsstuderende-de-mest-kaephoeje/

library("ggplot2")
library("foreign")

# Data er hentet fra: https://figshare.com/articles/SchulzTh_ni2016PONE_zip/2009238
st.data <- read.dta("SchulzThöni2016PONE.dta")

st.err <- function(x) {
  sd(x)/sqrt(length(x))
}

agg.err <- aggregate(confidence~study, st.data, st.err)[2]
names(agg.err) <- c("se")

st.agg <- cbind(aggregate(confidence~study, st.data, mean), agg.err)

st.agg$study <- factor(st.agg$study, levels=st.agg[order(st.agg$confidence), "study"])

png('ci-confidence.png', height=4, width=6, units="in", res=700)
ggplot(st.agg, aes(x=study, y=confidence)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean", fill="lightblue") +
  geom_errorbar(aes(ymin=confidence-se*1.96, ymax=confidence+se*1.96), width=.2, position=position_dodge(.9)) +
  theme_minimal() +
  xlab("") +
  ylab("Mean confidence (w. 95% CI)") +
  coord_flip() 
dev.off()
