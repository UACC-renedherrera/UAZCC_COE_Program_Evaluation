# set up ----
# load packages
library(here)
library(tidyverse)
library(zipcodeR)
library(leaflet)
library(ggthemes)
library(mapview)

# read data
events <- read_csv("data/raw/event_zip_code.csv",
                   col_names = c("location", "zipcode", "participants"),
                   skip = 1,
                   col_types = cols(
                     location = col_character(),
                     zipcode = col_character()
                   ))

# inspect
glimpse(events)

# mapping ----
# save zip codes
# save usa zip code to environment
zip_db <- zip_code_db

# save zip codes
zip_code_list <- as_tibble(events$zipcode)

zip_code_list <- rename(zip_code_list, "zipcode" = "value")

distinct(zip_code_list) %>%
  arrange(zipcode)

# match up zip codes from eval with usa database
zip_code_map <- inner_join(zip_code_list, zip_db, by = "zipcode") %>%
  drop_na()

# zip code list with county
zip_code_list_ <- zip_code_map %>%
  select(zipcode, county, state) %>%
  count(county) %>%
  arrange(desc(n))

# generate map
mapped_zipcodes <- leaflet(data = zip_code_map) %>%
  addTiles() %>%
  addMarkers(~lng, ~lat)

mapped_zipcodes

# maricopa county only
# match up zip codes from eval with usa database
zip_code_map_maricopa <- inner_join(zip_code_list, zip_db, by = "zipcode") %>%
  drop_na() %>%
  filter(county == "Maricopa County")

# generate map
mapped_zipcodes_maricopa <- leaflet(data = zip_code_map_maricopa) %>%
  addTiles() %>%
  addMarkers(~lng, ~lat)

mapped_zipcodes_maricopa

# catchment only
# match up zip codes from eval with usa database
zip_code_map_catch <- inner_join(zip_code_list, zip_db, by = "zipcode") %>%
  drop_na() %>%
  filter(county == "Cochise County" |
           county == "Pima County" |
           county == "Pinal County" |
           county == "Yuma County" |
           county == "Santa Cruz County")

# generate map
mapped_zipcodes_catch <- leaflet(data = zip_code_map_catch) %>%
  addTiles() %>%
  addMarkers(~lng, ~lat)

mapped_zipcodes_catch
