# R script for "Hvor står partierne i meningsmålingerne?"
# URL: https://erikgahner.dk/2023/hvor-star-partierne-i-meningsmalingerne/

library("tidyverse")
library("lubridate")

Sys.setlocale("LC_TIME", "da_DK.UTF-8")
options(OutDec= ",")

polls <- read_csv("polls.csv",
                  col_types = cols(
                    party_p = col_double(),
                    party_q = col_double(),
                    party_e = col_double(),
                    party_g = col_double(),
                    party_m = col_double(),
                    party_ae = col_double()
                  ))

polls <- polls |> 
  mutate(date = make_date(year, month, day),
         n = ifelse(is.na(n), 1000, n),
         across(starts_with("party"), ~ qbeta(1 - 0.05 / 2, n * (.x/100) + 1, n - n * (.x/100)) * 100, .names = "ci_max_{.col}"),
         across(starts_with("party"), ~ qbeta(0.05 / 2, n * (.x/100), n - n * (.x/100) + 1) * 100, .names = "ci_min_{.col}")
         ) |> 
  filter(date > as.Date("2022-11-01"))


polls |> 
  ggplot(aes(x=date, y=party_a)) + 
  geom_point(aes(colour = pollingfirm, shape = pollingfirm), size=2.5) +
  geom_errorbar(aes(colour = pollingfirm, ymin = ci_min_party_a, ymax = ci_max_party_a), width = 0, alpha = 0.5) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.title.y = element_text(hjust = 0),
    axis.title.x = element_text(hjust = 0),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  ) +
  labs(x = NULL,
       y = "Opbakning til Socialdemokratiet (%)",
       shape = NULL,
       colour = NULL) +
  scale_color_manual(values = c("#FF851B", "#FF4136", "#0074D9"))
  
ggsave("poll_03_A.png", width = 6, height = 4, bg = "white")

polls |> 
  ggplot(aes(x=date, y=party_f)) + 
  geom_point(aes(colour = pollingfirm, shape = pollingfirm), size=2.5) +
  geom_errorbar(aes(colour = pollingfirm, ymin = ci_min_party_f, ymax = ci_max_party_f), width = 0, alpha = 0.5) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.title.y = element_text(hjust = 0),
    axis.title.x = element_text(hjust = 0),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  ) +
  labs(x = NULL,
       y = "Opbakning til SF (%)",
       shape = NULL,
       colour = NULL) +
  scale_color_manual(values = c("#FF851B", "#FF4136", "#0074D9"))

ggsave("poll_03_F.png", width = 6, height = 4, bg = "white")

polls |> 
  ggplot(aes(x=date, y=party_v)) + 
  geom_point(aes(colour = pollingfirm, shape = pollingfirm), size=2.5) +
  geom_errorbar(aes(colour = pollingfirm, ymin = ci_min_party_v, ymax = ci_max_party_v), width = 0, alpha = 0.5) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.title.y = element_text(hjust = 0),
    axis.title.x = element_text(hjust = 0),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  ) +
  labs(x = NULL,
       y = "Opbakning til Venstre (%)",
       shape = NULL,
       colour = NULL) +
  scale_color_manual(values = c("#FF851B", "#FF4136", "#0074D9"))

ggsave("poll_03_V.png", width = 6, height = 4, bg = "white")

polls |> 
  ggplot(aes(x=date, y=party_i)) + 
  geom_point(aes(colour = pollingfirm, shape = pollingfirm), size=2.5) +
  geom_errorbar(aes(colour = pollingfirm, ymin = ci_min_party_i, ymax = ci_max_party_i), width = 0, alpha = 0.5) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.title.y = element_text(hjust = 0),
    axis.title.x = element_text(hjust = 0),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  ) +
  labs(x = NULL,
       y = "Opbakning til Liberal Alliance (%)",
       shape = NULL,
       colour = NULL) +
  scale_color_manual(values = c("#FF851B", "#FF4136", "#0074D9"))

ggsave("poll_03_I.png", width = 6, height = 4, bg = "white")

polls |> 
  ggplot(aes(x=date, y=noparty)) + 
  geom_point(aes(colour = pollingfirm, shape = pollingfirm), size=2.5) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.title.y = element_text(hjust = 0),
    axis.title.x = element_text(hjust = 0),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  ) +
  labs(x = NULL,
       y = "Respondenter uden partivalg (%)",
       shape = NULL,
       colour = NULL) +
  scale_color_manual(values = c("#FF851B", "#FF4136", "#0074D9"))

ggsave("poll_03_noparty.png", width = 6, height = 4, bg = "white")
