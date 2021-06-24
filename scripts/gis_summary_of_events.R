# display geospatial summary of events by zip code and/or census tract
# display geospatial summary of qualtrics contacts

# set up ----
# load library packages to environment
library(here)
library(tidyverse)
library(zipcodeR)
# library(leaflet)
library(ggthemes)
# library(mapview)
# library(curl)
library(geojsonio)
# library(rgdal)
library(tigris)
# library(ggmap)
# library(maps)
library(sf)

# options
options(tigris_use_cache = TRUE)
theme_set(theme_hc())

# read data ----
events <- read_csv("data/raw/event_zip_code.csv",
                   col_names = c("location", "zipcode", "participants"),
                   skip = 1,
                   col_types = cols(
                     location = col_character(),
                     zipcode = col_character()
                   )
)

# summarize event by zip code 
event_zip_summary <- events %>%
  group_by(zipcode) %>%
  summarize(participants = sum(participants),
            event_count = n()) %>%
  mutate(participants_per_event = participants/event_count)

# inspect
glimpse(event_zip_summary)

# load zip code data from zipcodeR
zip_db <- zip_code_db

# join zipcode db & event summary 
event_zip_summary <- inner_join(event_zip_summary, zip_db, by = "zipcode")

# inspect 
glimpse(event_zip_summary)

# select columns 
event_zip_summary <- event_zip_summary %>%
  select(1:4, 9:12, 16, 17)

# min, max, median, mean
event_zip_summary %>%
  summarize(min = min(participants),
            max = max(participants),
            median = median(participants),
            average = mean(participants))

# arrange to show highest count events 
event_zip_summary %>%
  arrange(desc(participants))

# histogram of event participant
event_zip_summary %>%
  ggplot(mapping = aes(x = participants)) +
  geom_histogram(bins = 100)

# remove outliers (connect 2 stem events)
event_zip_summary <- event_zip_summary %>%
  drop_na() %>%
  filter(participants < 6000)

# tigris zip codes 
tigris_zip_codes <- zctas(state = "04")

# inspect
glimpse(tigris_zip_codes)

# join data 
spatial_event_summary <- inner_join(
  event_zip_summary, tigris_zip_codes, by = c("zipcode" = "ZCTA5CE10")
)

write_rds(spatial_event_summary, "data/tidy/spatial_event_summary.rds")

# write shapefile 
st_write(
  obj = spatial_event_summary,
  dsn = "data/spatial/event_summary/event_summary.shp"
)
