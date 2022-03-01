# R script to figures in "Hvor mange vil stemme på Frie Grønne?"
# Link: https://erikgahner.dk/2022/hvor-mange-vil-stemme-pa-frie-gronne/

# Load packages
library("tidyverse")

Sys.setlocale("LC_TIME", "da_DK.UTF-8")

# Theme
theme_set(
  theme_grey(base_size = 11.5) %+replace% 
    theme(
      plot.margin = unit(rep(0.5, 4), "cm"), plot.background = element_blank(), panel.background = element_blank(),
      panel.border = element_blank(), legend.background = element_rect(fill = "transparent", colour = NA),
      legend.key = element_rect(fill = "transparent", colour = NA), legend.title = element_blank(),
      panel.grid.major = element_line(linetype = "dotted", colour = "#757575", size = 0.3), panel.grid.minor = element_blank(),
      axis.ticks = element_blank(), axis.line = element_line(color = "#FFFFFF", size = 0.3),
      plot.title = element_text(size = 12, hjust = 0, margin = margin(b = 15)),
      plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(b = 5)),
      plot.caption = element_text(size = 10, colour = "#212121", margin = margin(t = 15)),
      axis.title = element_text(size = 11, face = "plain"), axis.text = element_text(size = 10, face = "plain"),
      legend.text = element_text(size = 10), strip.text = element_text(size = 12, face = "plain")
    )
)

polls <- read_csv("https://raw.githubusercontent.com/erikgahner/polls/master/polls.csv",
                  col_types = cols(
                    party_e = col_double(),
                    party_g = col_double(),
                    party_p = col_double(),
                    party_q = col_double(),
                    party_moderaterne = col_double()
                  ))

polls <- polls %>% 
  mutate(
    date = as.Date(format(as.Date(c(paste(year, month, day, sep="-")), by = "days")))
  ) %>% 
  arrange(date) 

for(i in c("a", "b", "c", "d", "e", "f", "g", "i", "k", "o", "p", "v", "oe", "aa", "q", "moderaterne")) {
  polls <- within(polls, {
    assign(paste0("ci_", i), 1.96 * sqrt(( get(paste0("party_", i)) * (100 - get(paste0("party_", i)))) / n))
  }
  )
}


polls %>%
  drop_na(party_q) %>% 
  ggplot(aes(x=date, y = party_q, ymin = party_q - ci_q, ymax = party_q + ci_q, colour = pollingfirm)) + 
  geom_hline(yintercept = 2, linetype = "dashed") +
  geom_line() +
  geom_point(size=3) +
  geom_errorbar(width = 0) +
  labs(y = "Opbakning til Frie Grønne (%)",
       x = NULL) +
  theme(legend.position="bottom") +
  ggthemes::scale_color_colorblind() 

ggsave("friegroenne.png", width = 6, height = 4)

number_of_polls <- polls %>% drop_na(party_q) %>% nrow()

polls_aa <- polls %>% 
  drop_na(party_aa) %>% 
  head(number_of_polls) %>% 
  transmute(pollingfirm, date, support = party_aa, ci = ci_aa, party = "Alternativet (2015)")

polls_d <- polls %>% 
  drop_na(party_d) %>% 
  head(number_of_polls) %>% 
  transmute(pollingfirm, date, support = party_d, ci = ci_d, party = "Nye Borgerlige (2016)")

polls_e <- polls %>% 
  drop_na(party_e) %>% 
  head(number_of_polls) %>% 
  transmute(pollingfirm, date, support = party_e, ci = ci_e, party = "Klaus Riskær Pedersen (2019)")

polls_g <- polls %>% 
  drop_na(party_g) %>% 
  head(number_of_polls) %>% 
  transmute(pollingfirm, date, support = party_g, ci = ci_g, party = "Veganerpartiet (2020)")

polls_p <- polls %>% 
  drop_na(party_p) %>% 
  head(number_of_polls) %>% 
  transmute(pollingfirm, date, support = party_p, ci = ci_p, party = "Stram Kurs (2019)")

polls_moderaterne <- polls %>% 
  drop_na(party_moderaterne) %>% 
  head(number_of_polls) %>% 
  transmute(pollingfirm, date, support = party_moderaterne, ci = ci_moderaterne, party = "Moderaterne (2020)")

polls_q <- polls %>% 
  drop_na(party_q) %>% 
  head(number_of_polls) %>% 
  transmute(pollingfirm, date, support = party_q, ci = ci_q, party = "Frie Grønne (2020)")

newpolls <- bind_rows(polls_aa, polls_d, polls_e, polls_g, polls_p, polls_moderaterne, polls_q)

newpolls %>% 
  ggplot(aes(x=date, y = support, ymin = support - ci, ymax = support + ci, colour = pollingfirm)) + 
  geom_hline(yintercept = 2, linetype = "dashed") +
  geom_point(size=3) +
  geom_line(alpha = 0.5) +
  geom_errorbar(width = 0) +
  facet_wrap(~ party, scales = "free_x", ncol = 3) +
  ggthemes::scale_color_colorblind() +
  theme(legend.position="bottom") +
  labs(y = NULL,
       x = NULL) +
  scale_x_date(date_breaks = "1 month", 
               date_labels =  "%b")

ggsave("nyepartier.png", width = 8, height = 8)

