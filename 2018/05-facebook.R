# R script to "Why you should not trust the Facebook experiment"
# Link: http://erikgahner.dk/2018/why-you-should-not-trust-the-facebook-experiment/

library("ggplot2")

respondents <- 1095

df_fb <- data.frame(
  time = c(0, 0, 1, 1),
  tr = c("Treatment", "Control", "Treatment", "Control"),
  res = c(rep(respondents/2,2), 516, 372)
)

ggplot(df_fb, aes(x=time, y=res, group=tr, fill = tr)) +
  geom_bar(position="dodge", stat="identity", alpha=.8) +
  scale_y_continuous("Group size") +
  scale_x_continuous("", breaks=0:1, labels=
                       c("Pre", "Post")) +
  scale_fill_manual(values=c("#2679B2", "#E02527")) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.title=element_blank(), legend.position = "top") + 
  geom_segment(aes(x = 0.56, y = 516, xend = 0.56, yend = 372), linetype="dashed", colour="#2679B2") +
  geom_segment(aes(x = 0.56, y = 516, xend = 1, yend = 516), linetype="dashed", colour="#2679B2") 

ggsave("attrition.png", height=4, width=4)