# R script to table in "Honesty may still pay off in politics"
# Link: https://erikgahner.dk/2020/honesty-may-still-pay-off-in-politics/

library("tidyverse")
library("haven")
library("sjPlot")


# Download file at: https://doi.org/10.7910/DVN/MPAZUD
truth_data_raw <- read_dta("dataverse_files/dataRepl.dta")

truth_data <- truth_data_raw %>% 
  drop_na(interestreport) %>% 
  filter(!honestytime_cutoff %in% c(1, 3, 4))

results_1 <- lm(reelected ~ iscara, data = truth_data)

results_2 <- lm(reelected ~ iscara, data = filter(truth_data, ranagain == 1))

results_3 <- lm(reelected ~ iscara + ranagain, data = truth_data)

results_4 <- lm(reelected ~ iscara + ranagain + margin2015 + iscara:margin2015, data = truth_data)

tab_model(results_1, results_2, results_3, results_4,
          show.ci = FALSE,
          show.p = FALSE,
          collapse.se = TRUE,
          show.se = TRUE,
          show.intercept = FALSE,
          file = "truth.htm",
          pred.labels = c("Reported heads", 
                          "Ran for reelection",
                          "Margin 2015",
                          "Reported heads × margin 2015"),
          dv.labels = c("(1) Reproduction", "(2) Reelection sample", "(3) Reelection covariate", "(4) Interaction"))

