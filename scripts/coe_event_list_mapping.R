# set up ----
# load packages
library(here)
library(tidyverse)
library(zipcodeR)
library(leaflet)
library(ggthemes)
library(mapview)
library(curl)
library(geojsonio)
#library(rgdal)
library(tigris)
#library(ggmap)
#library(maps)
library(sf)

# options
options(tigris_use_cache = TRUE)
theme_set(theme_hc())

# load zip code data 
zip_db <- zip_code_db

# read data ----
events <- read_csv("data/raw/event_zip_code.csv",
                   col_names = c("location", "zipcode", "participants"),
                   skip = 1,
                   col_types = cols(
                     location = col_character(),
                     zipcode = col_character()
                   ))

# inspect
glimpse(events)

events <- inner_join(events, zip_db, by = "zipcode")

events <- events %>%
  select(location,
         zipcode,
         participants,
         county,
         state,
         population,
         lat,
         lng) %>%
  drop_na()

# exploratory
# events by zipcode for entire state
events_by_zipcode <- events %>% 
  group_by(zipcode) %>%
  summarise(sum_participants = sum(participants),
            mean_participants = mean(participants),
            median_participants = median(participants),
            min_participants = min(participants),
            max_participants = max(participants),
            event_count = n()
  ) %>%
  arrange(desc(event_count)) %>%
  drop_na()

# events by zipcode for catchment only 
events_by_zipcode_catchment <- events %>% 
  filter(county == "Cochise County" |
           county == "Pima County" |
           county == "Pinal County" |
           county == "Yuma County" |
           county == "Santa Cruz County") %>%
  group_by(zipcode) %>%
  summarise(sum_participants = sum(participants),
            mean_participants = mean(participants),
            median_participants = median(participants),
            min_participants = min(participants),
            max_participants = max(participants),
            event_count = n()
  ) %>%
  arrange(desc(event_count)) %>%
  drop_na()

# events by county for entire state
events_by_county <- events %>% 
  group_by(county) %>%
  summarise(sum_participants = sum(participants),
            mean_participants = mean(participants),
            median_participants = median(participants),
            min_participants = min(participants),
            max_participants = max(participants),
            event_count = n()
  ) %>%
  arrange(desc(event_count)) %>%
  drop_na()

# events by county for catchment only 
events_by_county_catchment <- events %>% 
  filter(county == "Cochise County" |
           county == "Pima County" |
           county == "Pinal County" |
           county == "Yuma County" |
           county == "Santa Cruz County") %>%
  group_by(county) %>%
  summarise(sum_participants = sum(participants),
            mean_participants = mean(participants),
            median_participants = median(participants),
            min_participants = min(participants),
            max_participants = max(participants),
            event_count = n()
  ) %>%
  arrange(desc(event_count)) %>%
  drop_na()


events_by_zipcode %>%
  ggplot(mapping = aes(sum_participants)) +
  geom_histogram(fill = "navy", color = "white",
                 binwidth = 100)

events_by_zipcode %>%
  ggplot(mapping = aes(mean_participants)) +
  geom_histogram(fill = "navy", color = "white",
                 binwidth = 50)

events_by_zipcode %>%
  ggplot(mapping = aes(median_participants)) +
  geom_histogram(fill = "navy", color = "white",
                 binwidth = 50)

events_by_zipcode %>%
  ggplot(mapping = aes(event_count)) +
  geom_histogram(fill = "navy", color = "white",
                 bins = 5)

# interactive mapping with leaflet ----
# markers ----
# showing pins of which zip codes had coe events and activities


# zip code list with county
events %>%
  select(zipcode, county, state) %>%
  count(county) %>%
  arrange(desc(n))

# generate map
events_by_zipcode_mapped <- leaflet(data = events) %>%
  addProviderTiles("Stamen.TonerLite") %>%
  addMarkers(~lng, ~lat)

events_by_zipcode_mapped

# maricopa county only
# match up zip codes from eval with usa database
events_by_zipcode_maricopa <- events %>%
  filter(county == "Maricopa County")

# generate map
events_by_zipcode_maricopa_mapped <- leaflet(data = events_by_zipcode_maricopa) %>%
  addProviderTiles("Stamen.TonerLite") %>%
  addMarkers(~lng, ~lat)

events_by_zipcode_maricopa_mapped

# catchment only
# match up zip codes from eval with usa database
events_by_zipcode_catchment <- events %>%
  filter(county == "Cochise County" |
           county == "Pima County" |
           county == "Pinal County" |
           county == "Yuma County" |
           county == "Santa Cruz County")

# generate map
events_by_zipcode_catchment_mapped <- leaflet(data = events_by_zipcode_catchment) %>%
  addProviderTiles("Stamen.TonerLite") %>%
  addMarkers(~lng, ~lat)

events_by_zipcode_catchment_mapped

# choropleth mapping ----
# showing number of participants engaged with COE activities by zip code
# download geojson az zipcode shapefile from github
# https://raw.githubusercontent.com/OpenDataDE/State-zip-code-GeoJSON/master/az_arizona_zip_codes_geo.min.json
# curl_download("https://raw.githubusercontent.com/OpenDataDE/State-zip-code-GeoJSON/master/az_arizona_zip_codes_geo.min.json",
#              destfile = "GIS/az_arizona_zip_codes_geo.min.json")

json_az_zipcodes <- geojson_read("GIS/az_arizona_zip_codes_geo.min.json", what = "sp")

# state of AZ; impact area
json_events_by_zipcode <- geo_join(json_az_zipcodes, 
                       events_by_zipcode, 
                       by_sp= "ZCTA5CE10", 
                       by_df = "zipcode",
                       how = "inner")

bins <- c(0, 40, 80, 150, 331, Inf)
# comment out because it is hard to see the difference
# pal <- colorBin(c("#81D3EB", "#378DBD", "#1E5288", "#0C234B", "#001C48"),
#                 domain = coe_events$sum_participants,
#                 bins = bins)

pal <- colorBin("YlGnBu",
                domain = json_events_by_zipcode$sum_participants,
                bins = bins)

popup_sb <- paste0("Participants: ", 
                   as.character(json_events_by_zipcode$sum_participants))

map_impact_sum <- json_events_by_zipcode %>%
  leaflet() %>%
  addProviderTiles(providers$Stamen.TonerLite) %>%
  addPolygons(data = json_events_by_zipcode,
              fillColor = ~pal(json_events_by_zipcode$sum_participants),
              fillOpacity = .8,
              popup = ~popup_sb,
              weight = 1,
              color = ~pal(json_events_by_zipcode$sum_participants)) %>%
  addLegend(pal = pal,
            values = json_events_by_zipcode$sum_participants,
            title = "Total participants")

map_impact_sum

mapshot(map_impact_sum, file = "figures/maps/event_participants_impact_choropleth.png")

# map catchment only
events_by_zipcode_catchment

events_by_zipcode_catchment %>%
  ggplot(mapping = aes(x = sum_participants)) +
  geom_histogram(binwidth = 100)

events_by_zipcode_catchment <- geo_join(json_az_zipcodes, 
                                        events_by_zipcode_catchment, 
                       by_sp= "ZCTA5CE10", 
                       by_df = "zipcode",
                       how = "inner")

bins <- c(0, 40, 80, 150, 331, Inf)
pal <- colorBin("YlGnBu",
                domain = events_by_zipcode_catchment$sum_participants,
                bins = bins)

popup_sb <- paste0("Participants: ", 
                   as.character(events_by_zipcode_catchment$sum_participants))

map_catchment_sum <- events_by_zipcode_catchment %>%
  leaflet() %>%
  addProviderTiles(providers$Stamen.TonerLite) %>%
  addPolygons(data = events_by_zipcode_catchment,
              fillColor = ~pal(events_by_zipcode_catchment$sum_participants),
              fillOpacity = .8,
              popup = ~popup_sb,
              weight = 1,
              color = "white") %>%
  addLegend(pal = pal,
            values = events_by_zipcode_catchment$sum_participants,
            title = "Total participants")

map_catchment_sum

mapshot(map_catchment_sum, file = "figures/maps/event_participants_catchment_choropleth.png")

# by county catchment only
events_by_county_catchment


# by county AZ state
coe_event_stats <- inner_join(events, zip_db, by = "zipcode")

coe_event_stats <- coe_event_stats %>%
  select(location,
         zipcode,
         participants,
         county)

glimpse(coe_event_stats)

coe_event_stats <- coe_event_stats %>%
  group_by(county) %>%
  summarise(sum_participants = sum(participants),
            mean_participants = mean(participants),
            median_participants = median(participants),
            min_participants = min(participants),
            max_participants = max(participants),
            event_count = n()
  ) %>%
  arrange(desc(event_count)) %>%
  drop_na()

glimpse(coe_event_stats)

coe_event_stats %>%
  filter(county != "Maricopa County") %>%
  ggplot(mapping = aes(x = sum_participants, y = county)) +
  geom_bar(stat = "identity")
  
# map
az_counties <- counties(state = 04)
glimpse(az_counties)

coe_event_stats_by_county <- geo_join(az_counties, 
         coe_event_stats, 
         by_sp= "NAMELSAD", 
         by_df = "county",
         how = "inner")

bins <- c(0, 100, 200, 600, 2000, Inf)
pal <- colorBin("YlGnBu",
                domain = coe_event_stats_by_county$sum_participants,
                bins = bins)

coe_event_stats_by_county_map <- coe_event_stats_by_county %>%
  leaflet() %>%
  addProviderTiles(providers$Stamen.TonerLite) %>%
  addPolygons(data = coe_event_stats_by_county,
              fillColor = ~pal(coe_event_stats_by_county$sum_participants),
              fillOpacity = .8,
              weight = 1,
              color = "white") %>%
  addLegend(pal = pal,
            values = coe_event_stats_by_county$sum_participants,
            title = "Total participants")

coe_event_stats_by_county_map

mapshot(coe_event_stats_by_county_map, file = "figures/maps/event_participants_impact_counties_choropleth.png")

# printable maps ----
# data 

az_zipcode_sf <- geojson_sf("GIS/az_arizona_zip_codes_geo.min.json")

coe_event_by_zip_code <- geo_join(az_zipcode_sf, 
                       event_stats, 
                       by_sp= "ZCTA5CE10", 
                       by_df = "zipcode",
                       how = "inner")

# testing
# tigris 
az_counties_tigris <- counties(state = 04)
class(az_counties_tigris)

az_counties_tigris <- st_as_sf(az_counties_tigris)

coe_event_by_zip_code %>%
  mutate(fill_value = cut_number(sum_participants, n = 5)) %>%
  ggplot() +
  geom_sf(data = az_counties_tigris) +
  geom_sf(mapping = aes(fill = fill_value))

