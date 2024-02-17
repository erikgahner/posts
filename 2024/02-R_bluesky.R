# R script for "Links I have shared on Bluesky"
# URL: https://erikgahner.dk/2024/links-i-have-shared-on-bluesky/

library("tidyverse")
library("atrrr")

skeets <- get_skeets_authored_by(actor = "erikgahner.bsky.social",
                                 parse = TRUE,
                                 limit = 99L)

skeets |>
  filter(nchar(text) > 5, as.Date(indexed_at) >= "2024-01-01") |>
  arrange(indexed_at) |>
  select(text) |>
  print(n = 100)
