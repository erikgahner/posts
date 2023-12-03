# R script for "Der er ikke valg i morgen #2"
# URL: https://erikgahner.dk/2023/der-er-ikke-valg-i-morgen-2/

library("tidyverse")

Sys.setlocale("LC_TIME", "da_DK.UTF-8")

theme_set(theme_minimal())
theme_replace(legend.position = "none")
theme_replace(axis.title = element_text(hjust = 0))
theme_replace(panel.grid.major = element_line(color = "gray95"))
theme_replace(panel.grid.minor = element_blank())

df_raw <- read_csv("polls.csv")

df <- df_raw |> 
  mutate(date = make_date(year, month, day)) |> 
  filter(date > "2022-11-01", pollingfirm == "Voxmeter") |> 
  mutate(support_regering = party_a + party_m + party_v) |> 
  select(date, noparty, support_regering, n)

df |> 
  ggplot(aes(date, noparty)) +
  geom_smooth(se = FALSE, colour = "#001f3f", size = 2, span = .8) +
  geom_point(size = 3, shape = 21, fill = "#001f3f", colour = "white") +
  labs(x = NULL,
       y = "Andel tvivlere →") +
  scale_y_continuous(limits = c(15, 35), breaks = seq(15, 35, 5), labels = c(seq(15, 30, 5), "35%"))

ggsave("noparty_01.png", width = 5, height = 4, bg = "white")

df |> 
  ggplot(aes(noparty, support_regering)) + 
  geom_point(size = 3, shape = 21, fill = "#001f3f", colour = "white") +
  labs(x = "Andel tvivlere →",
       y = "Opbakning til regeringen →") +
  scale_y_continuous(limits = c(min(df$support_regering - 1), max(df$support_regering + 1)), breaks = seq(35, 50, 5), labels = c(seq(35, 45, 5), "50%")) +
  scale_x_continuous(limits = c(min(df$noparty - 1), max(df$noparty + 1)), breaks = seq(15, 30, 5), labels = c(seq(15, 25, 5), "30%"))

ggsave("noparty_02.png", width = 5, height = 4, bg = "white")

df |> 
  mutate(n_real = n * (100-noparty) / 100) |> 
  mutate(ci = 1.96 * sqrt((support_regering * (100 - support_regering)) / n),
         ci_real = 1.96 * sqrt((support_regering * (100 - support_regering)) / n_real)) |> 
  ggplot(aes(date, support_regering)) +
  geom_point(size = 3, shape = 21, fill = "#001f3f", colour = "white") +
  geom_errorbar(aes(ymin = support_regering - ci_real, 
                    ymax = support_regering + ci_real), 
                width = 0, linewidth = 0.8, colour = "#FF4136") +
  geom_errorbar(aes(ymin = support_regering - ci, 
                    ymax = support_regering + ci), 
                width = 0, linewidth = 0.9, colour = "#001f3f") +
  labs(x = NULL,
       y = "Opbakning til regeringen →") +
  scale_y_continuous(limits = c(33, 55), breaks = seq(30, 55, 5), labels = c(seq(30, 50, 5), "55%")) 

ggsave("noparty_03.png", width = 6, height = 4, bg = "white")
  