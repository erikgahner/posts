# R script for "How effective is nudging? #2"
# URL: https://erikgahner.dk/2022/how-effective-is-nudging-2/

library("tidyverse")

# https://osf.io/78a5n/
df <- read_csv("mhhb_nma_data.csv")

df_d <- df |> 
  arrange(abs(cohens_d)) 

df_d |> 
  slice(1)

df_d |> 
  slice(2)

df_d |> 
  slice(3)

df_d |> 
  filter(publication_id == 146)

df |> 
  count(publication_id) |> 
  count(n)

df |> 
  group_by(publication_id) |> 
  summarise(cohens_d_max = abs(max(cohens_d, na.rm = TRUE)),
            cohens_d_min = abs(min(cohens_d, na.rm = TRUE)),
            n = n()) |> 
  cor()

df_d |> 
  slice(4, 5)

df_d |> 
  filter(publication_id == 188)

df |> 
  filter(year < 2008) |> 
  mutate(wansink = ifelse(str_detect(str_to_lower(reference), "wansink"), 1, 0)) |> 
  pull(wansink) |> 
  table()

df |> 
  filter(year < 2008) |> 
  arrange(cohens_d)

df |>  
  mutate(pre_2008 = ifelse(year < 2008, 1, 0)) |> 
  lm(formula = cohens_d ~ pre_2008) |> 
  summary()

df_d |> 
  slice(6)

df_d |> 
  filter(publication_id == 201)

