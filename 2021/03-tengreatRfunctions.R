# R script for "Ten great R functions"
# Link: https://erikgahner.dk/2021/ten-great-r-functions/

library("tidyverse")

# 1. forcats::fct_reorder()

fig_a <- mtcars %>% 
  slice(1:5) %>% 
  mutate(car = rownames(.)) %>%
  ggplot(aes(car, disp)) +
  geom_col() +
  coord_flip() +
  labs(title = "No fct_reorder()")

fig_b <- mtcars %>% 
  slice(1:5) %>% 
  mutate(car = rownames(.)) %>%
  mutate(car = fct_reorder(car, disp)) %>% 
  ggplot(aes(car, disp)) +
  geom_col() +
  coord_flip() +
  labs(title = "With fct_reorder()")

png('fct_reorder.png', width = 1200, height = 500, units = "px", res = 200)
gridExtra::grid.arrange(fig_a, fig_b, ncol = 2)
dev.off()

# 2: countrycode::countrycode()

library("countrycode")

countrycode(c("DK", "SE"), 
            origin = "iso2c", 
            destination = "country.name")

# 3. tidyr::separate_rows()

df <- tibble(
  country = c(1, 2),
  SDG = c("SDG 5,SDG 17,SDG 3", "SDG 1,SDG 2,SDG 3")
)

df %>% separate_rows(SDG,
                     sep = ",",
                     convert = TRUE)


# 4. tidyr::crossing()

crossing(country = c("Denmark", "Sweden"),
         year = 1965:2021,
         value = NA_real_)

# 5. stringi::stri_reverse()

x <- "snoitcnuf R taerg neT"

stringi::stri_reverse(x)

# 6. purrr::reduce()

reduce(list(df_1, df_2,
            df_3, df_4), 
       left_join, 
       by = c("iso2c", "year"))

# 7. dplyr::distinct()

df <- tibble(
  x = c(1, 1, 2, 2),
  y = c(1, 1, 2, 4)
) 

df %>% dplyr::distinct(x, .keep_all = TRUE)

# 8. fuzzyjoin::regex_left_join()

df_1 <- data.frame(
  country = c("Denmark", "denmark"),
  year = 2020:2021
)

df_2 <- data_frame(regex_country = c("[Dd]enmark"),
                   type = 1:2)

df_1 %>%
  fuzzyjoin::regex_inner_join(df_2, by = c(country = "regex_country"))

# 9. ggplot2::labs()

# 10. tidyr::drop_na()

df %>% 
  filter(!is.na(var1))

df %>% 
  drop_na(var1)