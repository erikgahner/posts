# R script for "A problem with survey data when studying social media"
# Link: https://erikgahner.dk/2021/a-problem-with-survey-data-when-studying-social-media/

library("tidyverse")
library("scales")
library("MASS")
library("stargazer")

# Download the data from: https://www.tandfonline.com/doi/suppl/10.1080/10584609.2017.1334726/suppl_file/upcp_a_1334726_sm5739.zip
ties <- read_csv("supplemental data file.csv")

theme_set(theme_bw())

fig_data <- data.frame(name = c(1:5),
                        value = c(
                          # Full data
                          NROW(ties[!is.na(ties$protest),]),
                          
                          # + Twitter and Facebook use
                          NROW(ties[ties$twgeneral > 0 & ties$fbgeneral > 0,]),
                          
                          # + Weak or strong ties
                          NROW(ties[ties$twgeneral > 0 & ties$fbgeneral > 0 & (ties$tw_wt > 0 | ties$tw_st > 0),]),
                          
                          # + Variation in ties
                          NROW(ties[ties$twgeneral > 0 & ties$fbgeneral > 0 & (ties$tw_wt > 0 | ties$tw_st > 0) & (ties$tw_wt != ties$tw_st) & (ties$fb_wt != ties$fb_st),]),
                          
                          # + Different ties
                          NROW(ties[ties$twgeneral > 0 & ties$fbgeneral > 0 & (ties$tw_wt > 0 | ties$tw_st > 0) & (ties$tw_wt != ties$tw_st) & (ties$fb_wt != ties$fb_st) & (ties$fb_wt != ties$tw_wt) & (ties$fb_st != ties$tw_st),])
                        ))


ggplot(fig_data, aes(x=name, y=value)) + 
  geom_bar(stat="identity") +
  scale_y_continuous("") +
  scale_x_continuous("", breaks=1:5, labels=c(paste0("Full data", "\n", "(100%)"), 
                                              paste0("+ Twitter and\n Facebook use", "\n(", percent(NROW(ties[ties$twgeneral > 0 & ties$fbgeneral > 0,]) /NROW(ties[!is.na(ties$protest),])), ")"),
                                              paste0("+ Weak or\n strong ties", "\n(", percent(NROW(ties[ties$twgeneral > 0 & ties$fbgeneral > 0 & (ties$tw_wt > 0 | ties$tw_st > 0),]) /NROW(ties[!is.na(ties$protest),])), ")"),
                                              paste0("+ Variation\n in ties", "\n(", percent(NROW(ties[ties$twgeneral > 0 & ties$fbgeneral > 0 & (ties$tw_wt > 0 | ties$tw_st > 0) & (ties$tw_wt != ties$tw_st) & (ties$fb_wt != ties$fb_st),]) /NROW(ties[!is.na(ties$protest),])), ")"),
                                              paste0("+ Different ties", "\n(", percent(NROW(ties[ties$twgeneral > 0 & ties$fbgeneral > 0 & (ties$tw_wt > 0 | ties$tw_st > 0) & (ties$tw_wt != ties$tw_st) & (ties$fb_wt != ties$fb_st) & (ties$fb_wt != ties$tw_wt) & (ties$fb_st != ties$tw_st),]) /NROW(ties[!is.na(ties$protest),])), ")"))
  )

ggsave("ties_data.png", width=5, height=4)


table1_m1 <- glm.nb(protest ~ fbgeneral + twgeneral + age + female + educ1 + interest + exteffic + leftist + news_std, data=ties)
table1_m1_w <- glm.nb(protest ~ fbgeneral + twgeneral + age + female + educ1 + interest + exteffic + leftist + news_std, data=ties, weights=weight)
table1_m2 <- glm.nb(protest ~ fbgeneral + twgeneral + fb_st + tw_st + fb_wt + tw_wt + age + female + educ1 + interest + exteffic + leftist + news_std, data=ties)
table1_m2_w <- glm.nb(protest ~ fbgeneral + twgeneral + fb_st + tw_st + fb_wt + tw_wt + age + female + educ1 + interest + exteffic + leftist + news_std, data=ties, weights=weight)

stargazer(table1_m1_w, table1_m1, table1_m2_w, table1_m2, type="text")
