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

# exploratory
events %>% 
  group_by(zipcode) %>%
  summarise(sum_participants = sum(participants)) %>%
  arrange(desc(sum_participants))

event_stats <- events %>% 
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

glimpse(event_stats)

event_stats %>%
  ggplot(mapping = aes(sum_participants)) +
  geom_histogram(fill = "navy", color = "white")
  
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


# download geojson az zipcode shapefile from github
# https://raw.githubusercontent.com/OpenDataDE/State-zip-code-GeoJSON/master/az_arizona_zip_codes_geo.min.json
curl_download("https://raw.githubusercontent.com/OpenDataDE/State-zip-code-GeoJSON/master/az_arizona_zip_codes_geo.min.json",
              destfile = "GIS/az_arizona_zip_codes_geo.min.json")

az_zipcodes <- geojson_read("GIS/az_arizona_zip_codes_geo.min.json", what = "sp")

az_zipcodes %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(popup = az_zipcodes$ZCTA5CE10)

coe_events <- geo_join(az_zipcodes, 
                       event_stats, 
                       by_sp= "ZCTA5CE10", 
                       by_df = "zipcode",
                       how = "inner")

pal <- colorBin("Reds",
                domain = coe_events$sum_participants,
                bins = 10)

popup_sb <- paste0("Participants: ", 
                   as.character(coe_events$sum_participants))

coe_events %>%
  leaflet() %>%
  addProviderTiles(providers$Stamen.TonerLite) %>%
  addPolygons(data = coe_events,
              fillColor = ~pal(coe_events$sum_participants),
              fillOpacity = .7,
              popup = ~popup_sb,
              weight = 1) %>%
  addLegend(pal = pal,
            values = coe_events$sum_participants)


az_spdf <- readOGR(
  dsn = "GIS/az_arizona_zip_codes_geo.min.json"
)

mypalette <- colorNumeric( palette="viridis", 
                           domain=event_stats$sum_participants, 
                           na.color="transparent")
mypalette(c(45,43))

leaflet(az_spdf) %>%
  addTiles() %>%
  addPolygons(fillColor = ~mypalette(event_stats$sum_participants),
              stroke = FALSE) %>%
  addLegend(pal = mypalette,
            values = ~event_stats$sum_participants)


# bins <- c(0, 250, 500, 750, 1000, Inf)
# pal <- colorBin("YlOrRd", domain = az_map$density, bins = bins)
# 
# labels <- sprintf(
#   "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
#   az_map$name, az_map$density
# ) %>% lapply(htmltools::HTML)
# 
# leaflet(data = az_map) %>%
#   addTiles() %>%
#   addPolygons(
#     fillColor = ~pal(density),
#     weight = 2,
#     opacity = 1,
#     color = "white",
#     dashArray = "3",
#     fillOpacity = 0.7,
#     highlight = highlightOptions(
#       weight = 5,
#       color = "#666",
#       dashArray = "",
#       fillOpacity = 0.7,
#       bringToFront = TRUE),
#     label = labels,
#     labelOptions = labelOptions(
#       style = list("font-weight" = "normal", padding = "3px 8px"),
#       textsize = "15px",
#       direction = "auto")) %>%
#   addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
#             position = "bottomright")


