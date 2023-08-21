# R script for "The sustainability impact focus of Nordic startups"
# URL: https://erikgahner.dk/2023/the-sustainability-impact-focus-of-nordic-startups/

library("tidyverse")
library("rvest")
library("ggsvg")

# Scrape data

get_startup_data <- function(x) {
  temp_html <- read_html(x)
  
  temp_name <- temp_html |> 
    html_nodes(".startup-header__name") |> 
    html_text()
  
  temp_info <- temp_html |> 
    html_nodes(".key-value-list") |> 
    html_table() |>
    as.data.frame() |> 
    sjmisc::rotate_df(cn = TRUE, rn = "row") 
  
  temp_sdg <- temp_html |> 
    html_nodes(".sdgs") |> 
    html_nodes("div") |> 
    html_nodes("img") |> 
    html_attr("src") |> 
    paste(collapse=";")
  
  temp_description <- temp_html |> 
    html_nodes(".text-block__content") |> 
    html_text() |> 
    as.data.frame() |> 
    slice(1) |> 
    pull()
  
  
  temp_all <- data.frame(name = temp_name,
                         temp_info,
                         sdgs = temp_sdg,
                         description = temp_description) |> 
    select(-row)
  
  return(temp_all)
  
}

sdg_01 <- readxl::read_xlsx("sdg_01.xlsx")
sdg_02 <- readxl::read_xlsx("sdg_02.xlsx")
sdg_03 <- readxl::read_xlsx("sdg_03.xlsx")
sdg_04 <- readxl::read_xlsx("sdg_04.xlsx")
sdg_05 <- readxl::read_xlsx("sdg_05.xlsx")
sdg_06 <- readxl::read_xlsx("sdg_06.xlsx")
sdg_07 <- readxl::read_xlsx("sdg_07.xlsx")
sdg_08 <- readxl::read_xlsx("sdg_08.xlsx")
sdg_09 <- readxl::read_xlsx("sdg_09.xlsx")
sdg_10 <- readxl::read_xlsx("sdg_10.xlsx")
sdg_11 <- readxl::read_xlsx("sdg_11.xlsx")
sdg_12 <- readxl::read_xlsx("sdg_12.xlsx")
sdg_13 <- readxl::read_xlsx("sdg_13.xlsx")
sdg_14 <- readxl::read_xlsx("sdg_14.xlsx")
sdg_15 <- readxl::read_xlsx("sdg_15.xlsx")
sdg_16 <- readxl::read_xlsx("sdg_16.xlsx")
sdg_17 <- readxl::read_xlsx("sdg_17.xlsx")

already_done <- unique(c(sdg_01$company_url, sdg_02$company_url, sdg_03$company_url,
                         sdg_04$company_url, sdg_05$company_url, sdg_06$company_url,
                         sdg_07$company_url, sdg_08$company_url, sdg_09$company_url, 
                         sdg_10$company_url, sdg_11$company_url, sdg_12$company_url, 
                         sdg_13$company_url, sdg_14$company_url, sdg_15$company_url,
                         sdg_16$company_url))


## Code used for all SDGs
sdg_17_df <- map_df(unique(sdg_17$company_url[!sdg_17$company_url %in% already_done]), get_startup_data)
sdg_17_df |> distinct(name, Website, .keep_all = TRUE) |> write_csv("sdg_17_df.csv")


# Create file with all SDGs
sdg_01_df <- read_csv("sdg_01_df.csv")

sdg_02_df <- read_csv("sdg_02_df.csv") |> 
  mutate(Founded = parse_number(Founded))

sdg_03_df <- read_csv("sdg_03_df.csv")

sdg_04_df <- read_csv("sdg_04_df.csv") |> 
  mutate(Founded = parse_number(Founded))

sdg_05_df <- read_csv("sdg_05_df.csv") 

sdg_06_df <- read_csv("sdg_06_df.csv") 

sdg_07_df <- read_csv("sdg_07_df.csv") 

sdg_08_df <- read_csv("sdg_08_df.csv") 

sdg_09_df <- read_csv("sdg_09_df.csv") |> 
  mutate(Founded = parse_number(Founded))

sdg_10_df <- read_csv("sdg_10_df.csv") 

sdg_11_df <- read_csv("sdg_11_df.csv") 

sdg_12_df <- read_csv("sdg_12_df.csv") 

sdg_13_df <- read_csv("sdg_13_df.csv") 

sdg_14_df <- read_csv("sdg_14_df.csv") 

sdg_15_df <- read_csv("sdg_15_df.csv") 

sdg_16_df <- read_csv("sdg_16_df.csv") 

sdg_17_df <- read_csv("sdg_17_df.csv")  |> 
  mutate(Founded = parse_number(Founded))

sdg_raw <- bind_rows(sdg_01_df, sdg_02_df, sdg_03_df, sdg_04_df,
                     sdg_05_df, sdg_06_df, sdg_07_df, sdg_08_df,
                     sdg_09_df, sdg_10_df, sdg_11_df, sdg_12_df,
                     sdg_13_df, sdg_14_df, sdg_15_df, sdg_16_df, 
                     sdg_17_df) |> 
  distinct(name, Website, .keep_all = TRUE)

sdg <- sdg_raw |> 
  mutate(SDG_01 = ifelse(str_detect(sdgs, "/_nuxt/492684d34299c698e0aff42dd021b7d6.svg"), 1, 0),
         SDG_02 = ifelse(str_detect(sdgs, "/_nuxt/38fa681661e416c4ba379795d03c2bb2.svg"), 1, 0),
         SDG_03 = ifelse(str_detect(sdgs, "/_nuxt/edd9986671672f80cd521c1e8de47bd7.svg"), 1, 0),
         SDG_04 = ifelse(str_detect(sdgs, "/_nuxt/792158d3c84c5e19e94885e7f47a2727.svg"), 1, 0),
         SDG_05 = ifelse(str_detect(sdgs, "/_nuxt/b461a42f95ae2d1d5fa122d7a2073412.svg"), 1, 0),
         SDG_06 = ifelse(str_detect(sdgs, "/_nuxt/dbb1f49e4b3178614043747c6b172ee6.svg"), 1, 0),
         SDG_07 = ifelse(str_detect(sdgs, "/_nuxt/f4042eb4a9711fcae6ea657d5728ec0f.svg"), 1, 0),
         SDG_08 = ifelse(str_detect(sdgs, "/_nuxt/53407a05575517858a467c432f25ced3.svg"), 1, 0),
         SDG_09 = ifelse(str_detect(sdgs, "/_nuxt/5ad8b6c154f6ad08924428537b89dfd8.svg"), 1, 0),
         SDG_10 = ifelse(str_detect(sdgs, "/_nuxt/a4a7c11a98524437a8e57dc7f7ecb83b.svg"), 1, 0),
         SDG_11 = ifelse(str_detect(sdgs, "/_nuxt/68160a8c7545790ed518bd87a15089d6.svg"), 1, 0),
         SDG_12 = ifelse(str_detect(sdgs, "/_nuxt/b5a4f8ecd2f13c06595fde19b1e8023e.svg"), 1, 0),
         SDG_13 = ifelse(str_detect(sdgs, "/_nuxt/bb613f4716450ddf91e2e5bcd0a37b1e.svg"), 1, 0),
         SDG_14 = ifelse(str_detect(sdgs, "/_nuxt/12f378f2a92906e8d4c0ea4d61d0bd26.svg"), 1, 0),
         SDG_15 = ifelse(str_detect(sdgs, "/_nuxt/662edba2bbd499839d4100bec2ac375b.svg"), 1, 0),
         SDG_16 = ifelse(str_detect(sdgs, "/_nuxt/937505f2c250930fde693906a8e930c5.svg"), 1, 0),
         SDG_17 = ifelse(str_detect(sdgs, "/_nuxt/a1fef3b365fbdd6dd4b82368f861e6a2.svg"), 1, 0)
  ) |> 
  select(-sdgs)

sdg |> 
  write_csv("sdg_all.csv")

# Analyse data

sdg_raw <- read_csv("sdg_all.csv")

sdg_colour <- function(x) {
  if(x == 1) { return("#E5243B") } 
  if(x == 2) { return("#DDA63A") } 
  if(x == 3) { return("#4C9F38") } 
  if(x == 4) { return("#C5192D") } 
  if(x == 5) { return("#FF3A21") } 
  if(x == 6) { return("#26BDE2") } 
  if(x == 7) { return("#FCC30B") } 
  if(x == 8) { return("#A21942") } 
  if(x == 9) { return("#FD6925") } 
  if(x == 10) { return("#DD1367") }
  if(x == 11) { return("#FD9D24") }
  if(x == 12) { return("#BF8B2E") }
  if(x == 13) { return("#3F7E44") }
  if(x == 14) { return("#0A97D9") }
  if(x == 15) { return("#56C02B") }
  if(x == 16) { return("#00689D") }
  if(x == 17) { return("#19486A") }
}

sdg_svg_01 <- paste(readLines('icons/TheGlobalGoals_Icons_Color_Goal_1.svg'), collapse = "\n")
sdg_svg_02 <- paste(readLines('icons/TheGlobalGoals_Icons_Color_Goal_2.svg'), collapse = "\n")
sdg_svg_03 <- paste(readLines('icons/TheGlobalGoals_Icons_Color_Goal_3.svg'), collapse = "\n")
sdg_svg_04 <- paste(readLines('icons/TheGlobalGoals_Icons_Color_Goal_4.svg'), collapse = "\n")
sdg_svg_05 <- paste(readLines('icons/TheGlobalGoals_Icons_Color_Goal_5.svg'), collapse = "\n")
sdg_svg_06 <- paste(readLines('icons/TheGlobalGoals_Icons_Color_Goal_6.svg'), collapse = "\n")
sdg_svg_07 <- paste(readLines('icons/TheGlobalGoals_Icons_Color_Goal_7.svg'), collapse = "\n")
sdg_svg_08 <- paste(readLines('icons/TheGlobalGoals_Icons_Color_Goal_8.svg'), collapse = "\n")
sdg_svg_09 <- paste(readLines('icons/TheGlobalGoals_Icons_Color_Goal_9.svg'), collapse = "\n")
sdg_svg_10 <- paste(readLines('icons/TheGlobalGoals_Icons_Color_Goal_10.svg'), collapse = "\n")
sdg_svg_11 <- paste(readLines('icons/TheGlobalGoals_Icons_Color_Goal_11.svg'), collapse = "\n")
sdg_svg_12 <- paste(readLines('icons/TheGlobalGoals_Icons_Color_Goal_12.svg'), collapse = "\n")
sdg_svg_13 <- paste(readLines('icons/TheGlobalGoals_Icons_Color_Goal_13.svg'), collapse = "\n")
sdg_svg_14 <- paste(readLines('icons/TheGlobalGoals_Icons_Color_Goal_14.svg'), collapse = "\n")
sdg_svg_15 <- paste(readLines('icons/TheGlobalGoals_Icons_Color_Goal_15.svg'), collapse = "\n")
sdg_svg_16 <- paste(readLines('icons/TheGlobalGoals_Icons_Color_Goal_16.svg'), collapse = "\n")
sdg_svg_17 <- paste(readLines('icons/TheGlobalGoals_Icons_Color_Goal_17.svg'), collapse = "\n")

sdg_df <- data.frame(
  x = 1:17, 
  sdg = c(sdg_svg_01, 
          sdg_svg_02,
          sdg_svg_03,
          sdg_svg_04,
          sdg_svg_05,
          sdg_svg_06,
          sdg_svg_07,
          sdg_svg_08,
          sdg_svg_09,
          sdg_svg_10,
          sdg_svg_11,
          sdg_svg_12,
          sdg_svg_13,
          sdg_svg_14,
          sdg_svg_15,
          sdg_svg_16,
          sdg_svg_17))

sdg <- sdg_raw |> 
  mutate(total_sdg = SDG_01 + SDG_02 + SDG_03 + SDG_04 + SDG_05 + SDG_06 + SDG_07 + SDG_08 + SDG_09 + SDG_10 + SDG_11 + SDG_12 + SDG_13 + SDG_14 + SDG_15 + SDG_16 + SDG_17)


sdg |> 
  select(starts_with("SDG_")) |> 
  pivot_longer(SDG_01:SDG_17) |> 
  filter(value == 1) |> 
  count(name) |> 
  arrange(desc(n))

sdg |> 
  select(starts_with("SDG_")) |> 
  pivot_longer(SDG_01:SDG_17) |> 
  filter(value == 1) |> 
  count(name) |> 
  ggplot(aes(name, n, fill = name)) +
  geom_col() +
  geom_point_svg(data = sdg_df, aes(x = x, y = -.3, fill = "SDG_01"), svg = sdg_df$sdg) +
  geom_text(aes(label = n, colour = name, y = n + 25), size = 3.5) +
  theme_void() +
  theme(legend.position = "none") +
  scale_fill_manual(values = as.character(map(1:17, sdg_colour))) +
  scale_colour_manual(values = as.character(map(1:17, sdg_colour))) +
  expand_limits(y = c(-1, 7)) +
  coord_flip()

ggsave("sdg_dist.png", width = 6, height = 6, bg = "white")

sdg |> 
  select(starts_with("SDG_")) |> 
  cor() |> 
  as.data.frame() |> 
  as_tibble(rownames = "SDG") |> 
  pivot_longer(SDG_01:SDG_17) |> 
  mutate(value = ifelse(value == 1, NA, value)) |> 
  drop_na(value) |> 
  ggplot(aes(SDG, name, fill = abs(value))) +
  geom_tile() +
  geom_text(aes(label = round(value, 2), colour = value), size = 3) +
  scale_colour_gradient(low = "#FF4136",
                        high = "#0074D9",
                        guide = "none") +
  scale_fill_gradient(low = "white",
                      high = "black",
                      guide = "none") +
  theme_void() +
  geom_point_svg(data = sdg_df, aes(x = x, y = -.3, fill = 1), svg = sdg_df$sdg) +
  geom_point_svg(data = sdg_df, aes(x = -.3, y = x, fill = 1), svg = sdg_df$sdg) +
  expand_limits(y = -1, x = -1)

ggsave("sdg_cor.png", width = 6, height = 6, bg = "white")

