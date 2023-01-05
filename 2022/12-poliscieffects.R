# R script for "Effect sizes in political science"
# URL: https://erikgahner.dk/2022/effect-sizes-in-political-science/

# Load package
library("tidyverse")
library("pwr")
library("kableExtra")
library("gt")

# Get rid of scientific notation
options(scipen=999)

# Load data 
mm <- read_csv("12-poliscieffects.csv")

# Key numbers
## Number of effect sizes
NROW(mm$n)
NROW(mm$n[is.na(mm$n)])
summary(mm$n)
sum(mm$n, na.rm = TRUE)

## Number of unique studies
NROW(unique(mm$studyid))

# Median sample size
median(mm$n, na.rm=TRUE)

# Get small, medium and large correlations
quantile(mm$r, c(0.25, 0.5, 0.75), na.rm=TRUE)
Hmisc::wtd.quantile(mm$r, probs = c(0.25, 0.5, 0.75), weights = log(mm$n), na.rm=TRUE)
Hmisc::wtd.quantile(mm$r, probs = c(0.25, 0.5, 0.75), weights = log(mm$n_win), na.rm=TRUE)

NROW(mm[mm$r >= .5,]) / NROW(mm)

# Get correlation between effect size AND log(n) and log(n_winsorized)
cor(mm$r, log(mm$n), use = "pairwise.complete.obs")
cor(mm$r, log(mm$n_win), use = "pairwise.complete.obs")

# Get correlation between effect size and year of study
cor(mm$r, mm$substudy_year, use = "pairwise.complete.obs")

mm |>
  mutate(study_name_sorted = fct_reorder(study_name, -r)) |> 
  ggplot(aes(x = study_name_sorted, y = r)) +
  geom_point(position = position_jitter(h = 0, w = 0.15), colour = "gray50", size = 0.75, alpha = 0.3) +
  geom_boxplot(fill = NA, outlier.shape = NA, color = '#003366', fatten = 1, lwd = 0.25) +
  coord_flip() +
  labs(y = "Effect size (r)", 
       x = NULL) +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "gray95"))

ggsave("cor_studies.png", height = 6, width = 6, bg = "white")

mm_table <- mm |>
  group_by(study_name) |> 
  summarise(totalN = round(sum(n, na.rm = TRUE), 0),
            n = n(),
            low = quantile(r, c(0.25), na.rm=TRUE),
            medium = quantile(r, c(0.5), na.rm=TRUE),
            high = quantile(r, c(0.75), na.rm=TRUE)
  ) |> 
  select(study_name, n, totalN, low, medium, high) |> 
  mutate(study_url = case_when(
    study_name == "Aarøe et al. (2017)" ~ "https://doi.org/10.1017/S0003055416000770",
    study_name == "Amsalem and Zoizner (2022)" ~ "https://doi.org/10.1017/S0007123420000253",
    study_name == "Amsalem and Nir (2019)" ~ "https://doi.org/10.1177/0093650219866357",
    study_name == "Balliet (2010)" ~ "https://doi.org/10.1177/0022002709352443",
    study_name == "Benoit et al. (2003)" ~ "https://doi.org/10.1080/0363775032000179133",
    study_name == "Brugman et al. (2019)" ~ "https://doi.org/10.1017/langcog.2019.5",
    study_name == "Burke et al. (2013)" ~ "https://doi.org/10.1111/pops.12005",
    study_name == "Costa (2017)" ~ "https://doi.org/10.1017/XPS.2017.14",
    study_name == "Cruz (2017)" ~ "https://doi.org/10.1016/j.jenvp.2017.06.010",
    study_name == "D’Alessio and Allen (2000)" ~ "https://doi.org/10.1111/j.1460-2466.2000.tb02866.x",
    study_name == "de Wit and Bekkers (2017)" ~ "https://doi.org/10.1093/jopart/muw044",
    study_name == "Dinesen et al. (2019)" ~ "https://doi.org/10.1146/annurev-polisci-052918-020708",
    study_name == "Ditto et al. (2019)" ~ "https://doi.org/10.1177/1745691617746796",
    study_name == "Druckman (1994)" ~ "https://doi.org/10.1177/0022002794038003007",
    study_name == "Everett et al. (2020)" ~ "https://doi.org/10.1037/pspp0000286",
    study_name == "Frenken et al. (2022)" ~ "https://doi.org/10.1111/pops.12822",
    study_name == "Godefroidt (2022)" ~ "https://doi.org/10.1111/ajps.12692",
    study_name == "Jedinger and Burger (2021)" ~ "https://doi.org/10.1177/01461672211046808",
    study_name == "Jost et al. (2003)" ~ "https://doi.org/10.1037/0033-2909.129.3.339",
    study_name == "Lind and Boomgarden (2019)" ~ "https://doi.org/10.1080/23808985.2019.1614475",
    study_name == "Kivikangas et al. (2019)" ~ "https://psycnet.apa.org/doi/10.1037/bul0000308",
    study_name == "Lau et al. (2007)" ~ "https://doi.org/10.1111/j.1468-2508.2007.00618.x",
    study_name == "Luo et al. (2019)" ~ "https://doi.org/10.1177/1077699018804500",
    study_name == "Matthes et al. (2018)" ~ "https://doi.org/10.1177/0093650217745429",
    study_name == "Matthes et al. (2019)" ~ "https://doi.org/10.1080/10584609.2019.1619638",
    study_name == "O’Brochta (2019)" ~ "https://doi.org/10.1177/2053168018818232",
    study_name == "Paluck et al. (2018)" ~ "https://doi.org/10.1017/bpp.2018.25",
    study_name == "Sibley et al. (2012)" ~ "https://doi.org/10.1016/j.jrp.2012.08.002",
    study_name == "Sipma and Lubbers (2018)" ~ "https://doi.org/10.1057/s41269-018-0120-2",
    study_name == "Walter et al. (2019)" ~ "https://doi.org/10.1080/10584609.2019.1668894",
    study_name == "Yesilyurt and Yesilyurt (2019)" ~ "https://doi.org/10.1177/0022343318808841",
    study_name == "Zigerell (2018)" ~ "https://doi.org/10.1177/2053168017753862",
    study_name == "Zoizner (2018)" ~ "https://doi.org/10.1177/0093650218808691",
    TRUE ~ NA_character_
  )) |> 
  mutate(totalN = ifelse(totalN == 0, NA_real_, totalN)) 

mm_table |> 
  mutate(study = sprintf('<p align = "left"><a href = "%s">%s</a></p>', study_url, study_name),
         study = map(study, gt::html)) |> 
  select(study, n, totalN, low, medium, high) |> 
  gt() |> 
  data_color(
    columns = c(low, medium, high),
    colors = scales::col_numeric(
      palette = c("white", "black"),
      na.color = "white",
      domain = c(0, 1)
    )
  ) |> 
  fmt_number(c(low, medium, high), decimals = 2) |> 
  fmt_number(c(n, totalN), decimals = 0) |> 
  cols_label(
    study = "Study",
    n = "Effect sizes",
    totalN = "Total sample size",
    low = "L (25)",
    medium = "M (50)",
    high = "H (75)"
  ) |> 
  tab_spanner(
    label = "Effect sizes",
    columns = c(
      low, medium, high
    )
  ) |> 
  sub_missing(
    columns = everything(),
    rows = everything(),
    missing_text = "---"
  ) |> 
  as_raw_html()


power_test <- function(Q, P, S) {
  pwr.r.test(r=quantile(mm$r, na.rm=TRUE, Q), power = P, sig.level=S, alternative = c("two.sided"))
}

length(na.omit(mm$n[mm$n > power_test(0.25, 0.8, 0.05)$n])) / length(na.omit(mm$n))
