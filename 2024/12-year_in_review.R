# R script for "Year in review"
# URL: https://erikgahner.dk/2024/a-dataset-with-political-datasets-3/

library("tidyverse")

imdb_raw <- read_csv("ratings.csv")
rym_raw <- read_csv("erikgahner-music-export.csv")

imdb <- imdb_raw |>
    mutate(date = ymd(`Date Rated`)) |>
    filter(date >= "2024-01-01", date <= "2024-12-31")

imdb |>
    filter(`Title Type` == "Movie", `Your Rating` >= 8) |>
    arrange(`Original Title`) |>
    transmute(title_link = paste0("<a href='https://imdb.com/title/", Const, "/'>", `Original Title`, "</a>")) |>
    pull(title_link) |>
    paste0(collapse = ", ")

imdb_raw |>
    filter(`Title Type` == "Movie") |>
    mutate(year = year(`Date Rated`)) |>
    filter(year >= 2015 & year <= 2024) |>
    count(year) |>
    ggplot(aes(year, n, label = n)) +
    geom_col(fill = "gray70") +
    geom_text(aes(y = n - 10)) +
    scale_x_continuous(breaks = 2014:2024) +
    theme_minimal() +
    labs(x = NULL,
         y = "Number of movies")

ggsave("yearinreview_movies.png", width = 6, height = 4, bg = "white")

goodreads_raw |>
    mutate(year = year(`Date Added`)) |>
    filter(year == 2024) |>
    arrange(desc(`My Rating`))

goodreads_raw |>
    mutate(year = year(`Date Added`)) |>
    filter(year >= 2015 & year <= 2024) |>
    count(year) |>
    ggplot(aes(year, n, label = n)) +
    geom_col(fill = "gray70") +
    geom_text(aes(y = n - 5)) +
    scale_x_continuous(breaks = 2014:2024) +
    theme_minimal() +
    labs(x = NULL,
         y = "Number of books")

ggsave("yearinreview_books.png", width = 6, height = 4, bg = "white")

steam <- tibble(month = 1:12,
                playtime = c(0.09, 0.09, 0.12, 0.04, 0.01, 0.06, 0.04, 0.13, 0.02, 0.13, 0.17, 0.09))

steam |>
    ggplot(aes(month, playtime, label = round(playtime * 100))) +
    geom_line(colour = "gray70") +
    geom_point(size = 5, colour = "white") +
    geom_point(size = 2) +
    geom_text(aes(y = playtime + 0.015)) +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    scale_y_continuous(labels = scales::percent_format()) +
    theme_minimal() +
    theme(panel.grid.minor.x = element_blank()) +
    labs(x = NULL,
         y = "Play time (%)")

ggsave("yearinreview_steam.png", width = 6, height = 3, bg = "white")
