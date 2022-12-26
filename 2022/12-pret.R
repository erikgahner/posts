# R script for "Pret a Manger"
# URL: https://erikgahner.dk/2022/pret-a-manger/

library("tidyverse")
library("sf")
library("gt")
library("ggstar")

# https://postcodes.io/
# https://www.getthedata.com/open-postcode-geo-london
postcode_raw <- read_csv("open_postcode_geo_london.csv", col_names = FALSE)
postcode <- postcode_raw |> 
  setNames(c("postcode", "status", "usertype", "easting", "northing", "positional_quality_indicator", 
             "country", "latitude", "longitude", "postcode_no_space", "postcode_fixed_width_seven", 
             "postcode_fixed_width_eight", "postcode_area", "postcode_district", 
             "postcode_sector", "outcode", "incode", "local_authority_code", "local_authority_name")) |> 
  select(postcode, latitude, longitude)

pret_raw <- read_csv("12-pret.csv")

pret_visited <- pret_raw |> 
  transmute(postcode, visited = 1) |> 
  left_join(postcode, by = "postcode") 

# https://data.london.gov.uk/dataset/statistical-gis-boundary-files-london
london <- read_sf("ESRI/LSOA_2011_London_gen_MHW.shp")

ggplot() +
  geom_sf(data = st_transform(london, st_crs(4326)), fill = "white", colour = "lightgrey") +
  geom_star(data = drop_na(pret_visited, longitude, latitude), mapping = aes(longitude, latitude),
            colour = "#84182b", fill = "#84182b") +
  coord_sf(ylim = c(min(pret_visited$latitude, na.rm = TRUE) - 0.001, max(pret_visited$latitude, na.rm = TRUE) + 0.001),
           xlim = c(min(pret_visited$longitude, na.rm = TRUE) - 0.001, max(pret_visited$longitude, na.rm = TRUE) + 0.001)) +
  labs(y = NULL,
       x = NULL) +
  theme_void()

ggsave("pret_map.png", width = 6, height = 5, bg = "white")

pret <- pret_raw |> 
  mutate(quality_total = (quality_atmosphere + quality_size + quality_seating))

pret |> 
  arrange(desc(quality_total)) |> 
  select(-c("address", "postcode", "staff_male", "quality_service"), -starts_with("wifi_")) |> 
  gt() |> 
  cols_label(name = "",
             quality_atmosphere = "Atmosphere",
             quality_size = "Size",
             quality_seating = "Seating",
             quality_total = "Aggregated") |> 
  gtExtras::gt_theme_espn() |> 
  data_color(
    columns = quality_total,
    colors = scales::col_numeric(
      palette = c("white", "#84182b"),
      na.color = "white",
      domain = c(1, 30))
  ) |> 
  as_raw_html()
