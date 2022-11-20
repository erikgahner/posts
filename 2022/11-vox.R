# R script for "Donald Trump’s defeat and electoral contagion"
# URL: https://erikgahner.dk/2022/donald-trumps-defeat-and-electoral-contagion/

library("tidyverse")

# https://europeelects.eu/data/
df_raw <- read_csv("es.csv")

vox <- df_raw |> 
  filter(Scope == "National") |> 
  transmute(pollingfirm = `Polling Firm`, 
            date = `Fieldwork End`, n = parse_number(`Sample Size`),
            support = parse_number(Vox)) |> 
  filter(date < as.Date("2020-11-07") + 30, date > as.Date("2020-11-07") - 30) |> 
  mutate(ci = 1.96 * sqrt((support * (100 - support)) / n)) |> 
  droplevels()

vox |> 
  ggplot(aes(date, support, ymin = support - ci, ymax = support + ci, colour = pollingfirm)) +
  geom_vline(xintercept = as.Date("2020-11-07"), linetype = "dotted") +
  geom_point() +
  geom_line(alpha = .3) +
  geom_errorbar() +
  ggthemes::scale_colour_pander() +
  theme_minimal() +
  theme(legend.position = "bottom") +
  ylim(min(vox$support, na.rm = TRUE) - 6, max(vox$support, na.rm = TRUE) + 6) +
  labs(y = "Support for VOX (%)",
       x = "Days before and after Trump losing in 2020, Spain",
       colour = NULL) 

ggsave("vox.png", width = 7, height = 4)