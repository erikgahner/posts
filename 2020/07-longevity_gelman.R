# R script to figures in "A response to Andrew Gelman"
# Link: https://erikgahner.dk/2020/a-response-to-andrew-gelman/

# Load tidyverse (to make life easy)
library("tidyverse")

theme_set(theme_bw())


# Load the data
df_rdd <- data.table::fread("longevity.csv")

# Make the data ready for the analysis
df_m <- df_rdd %>% 
  filter(year >= 1945, living_day_imp_post > 0) %>% 
  mutate(won = ifelse(margin_pct_1 >= 0, 1, 0),
         margin = margin_pct_1)

# Run regression
df_m %>% 
  filter(abs(margin) < 5) %>% 
  lm(living_day_imp_post ~ won + margin, data = .) %>% 
  broom::tidy()


df_rdd <- data.table::fread("longevity.csv")
death_date <- sapply(df_rdd[,"death_date_imp"], as.character) 
living <- df_rdd[,"living"] == "yes"
death_date[living] <- "2020-01-01"
election_year <- as.vector(unlist(df_rdd[,"year"]))
election_date <- paste(election_year, "-11-05", sep="")
more_days <- as.vector(as.Date(death_date) - as.Date(election_date))
more_years <- more_days/365.24
age <- as.vector(unlist(df_rdd[,"living_day_imp_pre"]))/365.24
n <- nrow(df_rdd)
name <- paste(unlist(df_rdd[,"cand_last"]), unlist(df_rdd[,"cand_first"]), unlist(df_rdd[,"cand_middle"]))
first_race <- c(TRUE, name[2:n] != name[1:(n-1)])
margin <- as.vector(unlist(df_rdd[,"margin_pct_1"]))
won <- ifelse(margin > 0, 1, 0)
lifetime <- age + more_years
decades_since_1950 <- (election_year - 1950)/10
data <- data.frame(margin, won, election_year, age, more_years, living, lifetime, decades_since_1950)
subset <- first_race & election_year >= 1945 & election_year <= 2012 & abs(margin) < 10 & !living
library("arm")
fit_1a <- lm(more_years ~ won + age + decades_since_1950 + margin, data=data, subset=subset) 
display(fit_1a)


subset_reported <- election_year >= 1945 & election_year <= 2012 & abs(margin) < 10 & !living
fit_1a_reported <- lm(more_years ~ won + age + decades_since_1950 + margin, data=data, subset=subset_reported) 
display(fit_1a_reported)


rdrobust::rdrobust(y = df_m$living_day_imp_post, 
                   x = df_m$margin) %>% 
  summary()

rdrobust::rdrobust(y = df_m$living_day_imp_post, 
                   x = df_m$margin, 
                   covs = df_m$living_day_imp_pre) %>% 
  summary()


479 > 311

hall <- haven::read_dta("primary_analysis.dta")

hall_data <- hall %>% 
  filter(margin <= .2, absdist > .1095)


fig_a <- hall_data %>% 
  ggplot(aes(rv, dv)) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(y = NULL,
       x = NULL,
       title = "Raw data")

fig_b <- hall_data %>% 
  ggplot(aes(rv, dv)) +
  geom_point() +
  geom_smooth(se = FALSE, colour = "black") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(y = NULL,
       x = NULL,
       title = "Raw data with loess fit")


fig_c <- ggplot(hall_data, aes(rv, dv)) +
  geom_point() +
  geom_smooth(se = FALSE, colour = "black") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(y = NULL,
       x = NULL,
       title = "Raw data with seperate loess fits") +
  geom_smooth(data = filter(hall_data, rv < 0), colour = "red", se = FALSE) +
  geom_smooth(data = filter(hall_data, rv > 0), colour = "red", se = FALSE)

png("rdd-hall.png", height = 350, width = 950, units = "px", res = 90)
gridExtra::grid.arrange(fig_a, fig_b, fig_c, ncol = 3)
dev.off()