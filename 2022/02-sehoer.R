# R script for "Datavisualisering: Se & Hør-pigen"
# URL: https://erikgahner.dk/2022/datavisualisering-se-hoer-pigen/

library("tidyverse")
library("rvest")
library("gt")
library("plotDK")

sh_url <- c("https://hjemmestrik.dk/alle")

#sh_page <- read_html(sh_url)

sh_table <- html_nodes(sh_page, "table")

sh_raw <- sh_table %>% 
  html_table(fill=TRUE) |> 
  pluck(1)


#sh_raw |> write_csv("sh.csv")

sh_raw <- read_csv("sh.csv")

sh <- sh_raw |> 
  separate(`Højde og vægt`, into = c("kg", "hoejde"), sep = " / ") |> 
  separate(`Bryst, talje og hofte`, into = c("bryst", "talje", "hofte"), sep = ", ") |> 
  transmute(
    uge = Uge,
    year = År,
    navn = Navn,
    by = By,
    livret = Livret,
    kg = parse_number(kg),
    hoejde = parse_number(hoejde),
    bryst = parse_number(bryst),
    talje = parse_number(talje),
    hofte = parse_number(hofte)
  ) |> 
  filter(year > 1997) |> 
  mutate(kg = case_when(
    kg > 200 ~ kg / 10,
    kg == 162 ~ NA_real_,
    TRUE ~ kg
  )) |> 
  mutate(kg = ifelse(kg < 30, NA, kg)) |> 
  mutate(hoejde = case_when(
    hoejde < 50 ~ hoejde * 10,
    TRUE ~ hoejde
  )) |> 
  mutate(hofte = ifelse(hofte < 10, NA, hofte)) |> 
  mutate(tid = as.Date(paste(year, uge, 1, sep="-"), "%Y-%U-%u")) |> 
  mutate(bmi = kg / (hoejde/100)^2) |> 
  mutate(livret_kat = case_when(
    str_detect(str_to_lower(livret), "sushi") ~ "Sushi",
    str_detect(str_to_lower(livret), "mexica") ~ "Mexicansk",
    str_detect(str_to_lower(livret), "kines") ~ "Kinesisk",
    str_detect(str_to_lower(livret), "itali") ~ "Italiensk",
    str_detect(str_to_lower(livret), "flæskes") ~ "Flæskesteg",
    str_detect(str_to_lower(livret), "flæsk") ~ "Stegt flæsk",
    str_detect(str_to_lower(livret), "indis") ~ "Indisk",
    str_detect(str_to_lower(livret), "græsk") ~ "Græsk",
    str_detect(str_to_lower(livret), "wok") ~ "Wok",
    str_detect(str_to_lower(livret), "asiat") ~ "Asiatisk",
    str_detect(str_to_lower(livret), "pasta") ~ "Pastaret",
    str_detect(str_to_lower(livret), "gratis") ~ "Gratis mad",
    str_detect(str_to_lower(livret), "buffet") ~ "Buffet",
    str_detect(str_to_lower(livret), "bøf") ~ "Bøf",
    str_detect(str_to_lower(livret), "bacon") ~ "Bacon",
    str_detect(str_to_lower(livret), "afrika") ~ "Afrikansk",
    str_detect(str_to_lower(livret), "salat") ~ "Salat",
    str_detect(str_to_lower(livret), "thai") ~ "Thai",
    str_detect(str_to_lower(livret), "frikadeller") ~ "Frikadeller",
    str_detect(str_to_lower(livret), "fisk") ~ "Fisk",
    str_detect(str_to_lower(livret), "steak") ~ "Steak",
    str_detect(str_to_lower(livret), "burger") ~ "Burger",
    str_detect(str_to_lower(livret), "pizza") ~ "Pizza",
    str_detect(str_to_lower(livret), "lasagne") ~ "Lasagne",
    str_detect(str_to_lower(livret), "laks") ~ "Laks",
    str_detect(str_to_lower(livret), "japan") ~ "Japansk",
    str_detect(str_to_lower(livret), "boller i karry") ~ "Boller i karry",
    str_detect(str_to_lower(livret), "slik|flødeboller|vingummi|lakrids|karamel") ~ "Slik",
    str_detect(str_to_lower(livret), "kylling") ~ "Kylling",
    str_detect(str_to_lower(livret), "dansk") ~ "Dansk",
    str_detect(str_to_lower(livret), "dessert|jerry|pandekage|risalamande|paradisis") ~ "Dessert",
    livret == "is" ~ "Dessert",
    TRUE ~ livret
  )) |> 
  drop_na(bmi)

sh |> 
  add_count(livret_kat) |> 
  filter(n == 1)

sh |> 
  ggplot(aes(tid, bmi)) +
  geom_point(shape = 1) +
  theme_minimal() +
  labs(x = NULL,
       y = "BMI") +
  annotate("rect", xmin = min(sh$tid, na.rm = TRUE), xmax = max(sh$tid, na.rm = TRUE), 
           ymin = min(sh$bmi) - 1, ymax = 18.5,
           fill = "orange", alpha = .2) +
  annotate("rect", xmin = min(sh$tid, na.rm = TRUE), xmax = max(sh$tid, na.rm = TRUE), 
           ymin = 18.5, ymax = 25,
           fill = "green", alpha = .2) +
  annotate("rect", xmin = min(sh$tid, na.rm = TRUE), xmax = max(sh$tid, na.rm = TRUE), 
           ymin = 25, ymax = 30,
           fill = "orange", alpha = .2) +
  annotate("rect", xmin = min(sh$tid, na.rm = TRUE), xmax = max(sh$tid, na.rm = TRUE), 
           ymin = 30, ymax = max(sh$bmi) + 1,
           fill = "red", alpha = .2) 

ggsave("sh_bmi.png", width = 6, height = 3)


sh |> 
  count(livret_kat) |> 
  filter(nchar(livret_kat) > 1) |> 
  top_n(25) |> 
  mutate(livret_kat = fct_reorder(livret_kat, n)) |> 
  ggplot(aes(livret_kat, n)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(x = NULL,
       y = NULL)

ggsave("sh_livret.png", width = 6, height = 4)


Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

sh |> 
  filter(!year %in% c(1998, 2022)) |> 
  group_by(year) |> 
  summarise(n = n(),
            kg = median(kg),
            hoejde = median(hoejde),
            bmi_median = median(bmi),
            bmi_min = min(bmi),
            bmi_max = max(bmi),
            livret = Mode(livret_kat)) |> 
  gt() |> 
  gtExtras::gt_theme_espn() |> 
  cols_label(
    year = "År",
    n = "Antal",
    kg = "Vægt (kg)",
    hoejde = "Højde (cm)",
    bmi_median = "BMI (median)",
    bmi_min = "BMI (min)",
    bmi_max = "BMI (max)"
  ) |> 
  fmt_number(
    columns = c(kg, hoejde),
    dec_mark = ",",
    sep_mark = ".",
    decimals = 0
  ) |> 
  fmt_number(
    columns = c(bmi_median, bmi_min, bmi_max),
    dec_mark = ",",
    sep_mark = ".",
    decimals = 1
  ) |> 
  as_raw_html()
  

sh |> 
  filter(!year %in% c(1998, 2022)) |> 
  mutate(livret_sushi = ifelse(livret_kat == "Sushi", 1, 0)) |> 
  group_by(year) |> 
  summarise(sushi = mean(livret_sushi, na.rm = TRUE)) |> 
  ggplot(aes(year, sushi)) +
  geom_point() +
  geom_line()

sh |> 
  filter(livret_kat %in% c("Fisk", "Slik")) |> 
  group_by(livret_kat) |> 
  summarise(kg = mean(kg),
            hoejde = mean(hoejde))

reg_df <- sh |> 
  add_count(livret_kat) |> 
  mutate(livret_kat = ifelse(n > 12, livret_kat, "Andet")) |> 
  select(livret_kat) |> 
  fastDummies::dummy_cols() |> 
  select(-livret_kat) |> 
  bind_cols(sh) |> 
  select(-livret_kat) |> 
  select(kg, hoejde, year, starts_with("livret_kat")) |> 
  select(-livret_kat_Andet)

lm(kg ~ ., data = reg_df) |> 
  broom::tidy(conf.int = TRUE) |> 
  filter(str_detect(term, "livret")) |>
  mutate(term = str_remove(term, "livret_kat_")) |> 
  mutate(term = str_remove_all(term, "`")) |> 
  mutate(term = fct_reorder(term, estimate)) |> 
  ggplot(aes(term, estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point() +
  geom_errorbar(width = 0) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  theme_minimal() +
  scale_y_continuous(breaks = -4:7, labels = -4:7) +
  labs(x = NULL,
       y = "Regressionskoefficient (m. 95% KI)")

ggsave("sh_reg.png", width = 7, height = 5)
  
sh
sh_postnummer <- read_csv("sh_postnummer.csv") |> 
  distinct(postnrnavn, .keep_all = TRUE)
sh_postnummer |> 
  filter(str_detect(kommunenavn, "Nykøbing"))

sh_postnummer |> 
  filter(str_detect(str_to_lower(postnrnavn), "falster"))

sh_kom <- sh |> 
  distinct(navn, by, .keep_all = TRUE) |> 
  mutate(by = case_when(
    str_detect(by, "Viby") ~ "Viby J",
    str_detect(str_to_lower(by), "københavn|kbh|nørrebro") ~ "København K",
    str_detect(str_to_lower(by), "østerbro") ~ "København K",
    str_detect(str_to_lower(by), "odense") ~ "Odense C",
    str_detect(str_to_lower(by), "århus|aarhus") ~ "Aarhus C",
    str_detect(str_to_lower(by), "amager") ~ "København S",
    str_detect(str_to_lower(by), "randers") ~ "Randers C",
    str_detect(str_to_lower(by), "lyngby") ~ "Kongens Lyngby",
    str_detect(str_to_lower(by), "aalborg|ålborg") ~ "Kongens Lyngby",
    str_detect(str_to_lower(by), "stoholm") ~ "Stoholm Jyll",
    str_detect(str_to_lower(by), "nykøbing falste") ~ "Nykøbing F",
    TRUE ~ by
  )) |> 
  left_join(sh_postnummer, by = c("by" = "postnrnavn")) |> 
  mutate(kommunekode = as.numeric(kommunekode)) |> 
  group_by(kommunenavn, kommunekode) |> 
  summarise(n = n(),
            .groups = "drop")

sh_pop <- read_csv2("sh_indbyggere.csv") |> 
  select(-kommunenavn) |> 
  rename(kommunekode = komnr)
sh_full <- sh_kom |> left_join(sh_pop, by = "kommunekode") |> 
  mutate(sh_capita = n / (indbyggere/10000))

sh_full |> 
  #arrange(desc(n))
  arrange(desc(sh_capita))

plotDK(data = rename(sh_full, `Se & Hør-piger\npr. 10.000 indbyggere` = sh_capita), id = "kommunekode", value = "Se & Hør-piger\npr. 10.000 indbyggere", show_missing = TRUE) +
  scale_fill_gradient(low = "white", high = "red", na.value = NA)

ggsave("sh_kommune.png", width = 8, height = 5)
