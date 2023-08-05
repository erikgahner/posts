# R script for "The quality of opinion polling in parliamentary elections"
# URL: https://erikgahner.dk/2023/the-quality-of-opinion-polling-in-parliamentary-elections/

library("tidyverse")
library("patchwork")
library("gt")

# Load ParlPoll data
df_raw <- read_csv("parlpoll.csv")

# Load ParlGov data
parlgov <- read_csv("view_party.csv")
elections <- read_csv("view_election.csv")

# Functions
accuracy <- function(p, v) { log((p / (100 - p)) * ( (100 - v) / v)) }
floor_dec <- function(x, level = 1) { round(x - 5*10^(-level-1), level) }
ceiling_dec <- function(x, level = 1) { round(x + 5*10^(-level-1), level) }

df <- df_raw |> 
  left_join(parlgov, by = c("country_name", "party_id")) |> 
  mutate(difference = vote_share - support,
         difference_abs = abs(vote_share - support),
         accurate = ifelse(support > 0 & vote_share > 0, accuracy(p = support, v = vote_share), NA),
         days = election_date - date) |> 
  mutate(se = sqrt( ( support * (100 - support )) / samplesize)) |> 
  mutate(support_min = floor_dec(support - 1.96 * se, 1),
         support_max = ceiling_dec(support + 1.96 * se, 1),
         within_CI = ifelse(vote_share >= support_min & vote_share <= support_max, 1, 0)) |> 
  mutate(weeks = as.numeric(floor(days / 7))) |> 
  mutate(method = case_when(
    method_text == "Internet" ~ "Internet",
    method_text == "Online" ~ "Internet",
    method_text == "Phone" ~ "Phone",
    method_text == "Landline" ~ "Phone",
    method_text == "IVR" ~ "Phone",
    method_text == "Phone and internet" ~ "Phone + Internet",
    str_detect(str_to_lower(method_text), "telephone/online") ~ "Phone + Internet",
    str_detect(method_text, " and ") ~ NA_character_,
    str_detect(str_to_lower(method_text), "phone") ~ "Phone",
    str_detect(str_to_lower(method_text), "ivr/telephone") ~ "Phone",
    str_detect(str_to_lower(method_text), "internet") ~ "Internet",
    str_detect(str_to_lower(method_text), "online") ~ "Internet",
  ))

# Get number of unique polls
df |> 
  count(country_name, pollingfirm, date) |> 
  NROW()

# Get number of countries
df |> 
  count(country_name) |> 
  NROW()

# Get number of party-election observations
NROW(df)

# Get information on proportion of elections covered in a given year
df_cover <- df |> 
  distinct(country_name, election_date) |> 
  mutate(covered = "Yes")
df_cover <- elections |> 
  filter(election_type != "ep") |> 
  count(country_name, election_date) |> 
  left_join(df_cover, by = c("country_name", "election_date")) |> 
  mutate(covered = ifelse(is.na(covered), "No", covered))
df_cover |> 
  mutate(covered = ifelse(covered == "Yes", 1, 0)) |> 
  mutate(year = year(election_date)) |> 
  group_by(year) |> 
  summarise(covered = mean(covered)) |> 
  arrange(desc(year)) |> 
  print(n = 15)

# Cover of opinion polls since 1945
df_cover |> 
  filter(year(election_date) >= 1945) |> 
  pull(covered) |> 
  table()

df_cover |> 
  filter(year(election_date) >= 2000) |> 
  pull(covered) |> 
  table()


# Create figure with the distribution of polls
fig_freq_1 <- df |> 
  count(days, country_name, election_date, pollingfirm) |> 
  ggplot(aes(as.numeric(days))) +
  geom_histogram(bins = 40, fill = "lightgray", colour = "gray") +
  theme_minimal() +
  theme(axis.title.y = element_text(hjust = 0),
        axis.title.x = element_text(hjust = 0),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank()
  ) +
  labs(x = "Days until election →",
       y = "Number of opinion polls →",
       colour = NULL)

fig_freq_2 <- df |> 
  count(date, country_name, election_date, pollingfirm) |> 
  ggplot(aes(date)) +
  geom_histogram(bins = 40, fill = "lightgray", colour = "gray") +
  theme_minimal() +
  theme(axis.title.y = element_text(hjust = 0),
        axis.title.x = element_text(hjust = 0),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank()
  ) +
  labs(x = "Date →",
       y = NULL,
       colour = NULL)

fig_freq_1 + fig_freq_2

ggsave("parlpoll_freq.png", width = 7, height = 4, bg = "white")

# Get correlations between key variables
df |> 
  filter(days <= 3) |> 
  select(support, difference_abs, vote_share, samplesize, within_CI) |> 
  cor(use = "pairwise.complete.obs") |> 
  round(2)

# Create figure with correlation over time
df |> 
  drop_na(vote_share, support) |> 
  group_by(weeks) |> 
  summarise(cor = cor(support, vote_share), 
            n = n()) |> 
  filter(weeks < 52*4) |> 
  ggplot(aes(weeks, cor)) +
  geom_line() +
  geom_point(colour = "white", size = 2.1) +
  geom_point(size = 1.3) +
  ylim(c(0.7, 1)) +
  theme_minimal() + 
  theme(axis.title.y = element_text(hjust = 0),
        axis.title.x = element_text(hjust = 0),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank()
  ) +
  labs(x = "Weeks until election →",
       y = "Correlation between opinion poll and election result →",
       colour = NULL)

ggsave("parlpoll_weeks.png", width = 7, height = 5, bg = "white")

df |> 
  drop_na(vote_share) |> 
  filter(days <= 3) |> 
  mutate(within_CI = case_when(
    within_CI == 0 ~ "Outside 95% CI",
    within_CI == 1 ~ "Within 95% CI",
    TRUE ~ "No info on sample size"
  )) |> 
  drop_na(vote_share, support) |> 
  ggplot(aes(support, vote_share, fill = within_CI)) +
  geom_function(fun = function(x) x + 1.96 * sqrt( ( x * (100 - x )) / median(df$samplesize, na.rm = TRUE) ),
                linetype = "dashed", colour = "black") +
  geom_function(fun = function(x) x - 1.96 * sqrt( ( x * (100 - x )) / median(df$samplesize, na.rm = TRUE) ),
                linetype = "dashed", colour = "black") +
  geom_point(alpha = 0.7,
             shape = 21, colour = "white") +
  scale_fill_manual(values = c("grey50", "#F012BE", "#2ECC40")) +
  theme_minimal() +
  theme(legend.position = c(0.22, 0.88),
        legend.background = element_rect(fill = "white", color = "black"),
        axis.title.y = element_text(hjust = 0),
        axis.title.x = element_text(hjust = 0),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank()
  ) +
  labs(x = "Support in opinion poll (%) →",
       y = "Election result (%) →",
       fill = NULL)

ggsave("parlpoll_cor.png", width = 6, height = 6, bg = "white")

df_pol_means <- df |> 
  filter(days <= 3) |> 
  filter(family_name %in% c("Christian democracy", "Conservative", "Communist/Socialist", "Green/Ecologist",
                            "Liberal", "Right-wing", "Social democracy", "Special issue")) |> 
  group_by(family_name) |> 
  summarise(accurate_mean = mean(accurate, na.rm = TRUE))

df |> 
  drop_na(vote_share) |> 
  filter(days <= 3) |> 
  filter(family_name %in% c("Christian democracy", "Conservative", "Communist/Socialist", "Green/Ecologist",
                            "Liberal", "Right-wing", "Social democracy", "Special issue")) |> 
  ggplot(aes(accurate, fill = family_name)) +
  geom_histogram(colour = "white", size = .1) +
  geom_vline(data = df_pol_means,
             aes(xintercept = accurate_mean),
             linetype = "dashed",
             colour = "gray30") +
  facet_wrap(~ family_name, scales = "free_y", ncol = 4) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.y = element_text(hjust = 0),
        axis.title.x = element_text(hjust = 0),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank() 
  ) +
  labs(x = "Accuracy",
       y = NULL)

ggsave("parlpoll_familybias.png", width = 7, height = 4, bg = "white")

pollbias_cntry <- df |> 
  filter(days <= 3) |> 
  drop_na(vote_share, support) |> 
  group_by(election_date, country_name) |> 
  summarise(MAE = mean(difference_abs, na.rm = TRUE),
            RMSE = sqrt(mean((vote_share - support)^2, na.rm = TRUE)),
            partypoll = n(),
            .groups = "drop") |> 
  group_by(country_name) |> 
  summarise(MAE = weighted.mean(MAE, w = partypoll),
            RMSE = weighted.mean(RMSE, w = partypoll),
            partypoll = sum(partypoll),
            elections = n(),
            .groups = "drop") |> 
  arrange(MAE) |> 
  mutate(country_ico = countrycode::countrycode(country_name, origin = "country.name", destination = "iso2c")) 

pollbias_cntry |> 
  filter(elections > 1) |> 
  slice(1:5) |> 
  gt() |>
  cols_move_to_start(columns = country_ico) |>
  cols_label(
    country_ico = "",
    country_name = "Country",
    MAE = "MAE",
    partypoll = "Estimates",
    elections = "Elections"
  ) |>
  fmt_number(columns = c(MAE, RMSE), decimals = 2) |>
  fmt_flag(columns = country_ico) |> 
  as_raw_html()
