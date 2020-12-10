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
# library(rgdal)
library(tigris)
# library(ggmap)
# library(maps)
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
  )
)

# inspect
glimpse(events)

events <- inner_join(events, zip_db, by = "zipcode")

events <- events %>%
  select(
    location,
    zipcode,
    participants,
    county,
    state,
    population,
    lat,
    lng
  ) %>%
  drop_na()

# exploratory ----
# events by zipcode for entire state
events_by_zipcode <- events %>%
  group_by(zipcode) %>%
  arrange(zipcode) %>%
  drop_na()

# events by zipcode stats for entire state
events_by_zipcode_stats <- events %>%
  group_by(zipcode) %>%
  summarise(
    sum_participants = sum(participants),
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
  arrange(zipcode) %>%
  drop_na()

# events by zipcode stats for catchment only
events_by_zipcode_catchment_stats <- events %>%
  filter(county == "Cochise County" |
    county == "Pima County" |
    county == "Pinal County" |
    county == "Yuma County" |
    county == "Santa Cruz County") %>%
  group_by(zipcode) %>%
  summarise(
    sum_participants = sum(participants),
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
  arrange(county) %>%
  drop_na()

# events by county stats for entire state
events_by_county_stats <- events %>%
  group_by(county) %>%
  summarise(
    sum_participants = sum(participants),
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
  arrange(county) %>%
  drop_na()

# events by county stats for catchment only
events_by_county_catchment_stats <- events %>%
  filter(county == "Cochise County" |
    county == "Pima County" |
    county == "Pinal County" |
    county == "Yuma County" |
    county == "Santa Cruz County") %>%
  group_by(county) %>%
  summarise(
    sum_participants = sum(participants),
    mean_participants = mean(participants),
    median_participants = median(participants),
    min_participants = min(participants),
    max_participants = max(participants),
    event_count = n()
  ) %>%
  arrange(desc(event_count)) %>%
  drop_na()

# histograms
# all events in AZ by zipcode
events_by_zipcode_stats %>%
  ggplot(mapping = aes(sum_participants)) +
  geom_histogram(
    fill = "navy", color = "white",
    binwidth = 100
  )

# events in catchment by zipcode
events_by_zipcode_catchment_stats %>%
  ggplot(mapping = aes(sum_participants)) +
  geom_histogram(
    fill = "navy", color = "white",
    binwidth = 100
  )

events_by_zipcode_stats %>%
  ggplot(mapping = aes(mean_participants)) +
  geom_histogram(
    fill = "navy", color = "white",
    binwidth = 50
  )

events_by_zipcode_stats %>%
  ggplot(mapping = aes(median_participants)) +
  geom_histogram(
    fill = "navy", color = "white",
    binwidth = 50
  )

events_by_zipcode_stats %>%
  ggplot(mapping = aes(event_count)) +
  geom_histogram(
    fill = "navy", color = "white",
    bins = 5
  )

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
# combine json with stats
json_events_by_zipcode <- geo_join(json_az_zipcodes,
  events_by_zipcode_stats,
  by_sp = "ZCTA5CE10",
  by_df = "zipcode",
  how = "inner"
)

# set bins
bins <- c(0, 40, 80, 150, 331, Inf)
# comment out because it is hard to see the difference
# pal <- colorBin(c("#81D3EB", "#378DBD", "#1E5288", "#0C234B", "#001C48"),
#                 domain = coe_events$sum_participants,
#                 bins = bins)

# set palette
pal <- colorBin("YlGnBu",
  domain = json_events_by_zipcode$sum_participants,
  bins = bins
)

# set popup text
popup_sb <- paste0(
  "Participants: ",
  as.character(json_events_by_zipcode$sum_participants)
)

# generate leaflet map
map_impact_sum <- json_events_by_zipcode %>%
  leaflet() %>%
  addProviderTiles(providers$Stamen.TonerLite) %>%
  addPolygons(
    data = json_events_by_zipcode,
    fillColor = ~ pal(json_events_by_zipcode$sum_participants),
    fillOpacity = .8,
    popup = ~popup_sb,
    weight = 1,
    color = ~ pal(json_events_by_zipcode$sum_participants)
  ) %>%
  addLegend(
    pal = pal,
    values = json_events_by_zipcode$sum_participants,
    title = "Total participants"
  )

map_impact_sum

# save to disk
mapshot(map_impact_sum, file = "figures/maps/event_participants_impact_choropleth.png")

# map catchment only
# combine json with stats
json_events_by_zipcode_catchment <- geo_join(json_az_zipcodes,
  events_by_zipcode_catchment_stats,
  by_sp = "ZCTA5CE10",
  by_df = "zipcode",
  how = "inner"
)

# set bins
bins <- c(0, 40, 80, 150, 331, Inf)

# set palette
pal <- colorBin("YlGnBu",
  domain = json_events_by_zipcode_catchment$sum_participants,
  bins = bins
)

# set popup text
popup_sb <- paste0(
  "Participants: ",
  as.character(json_events_by_zipcode_catchment$sum_participants)
)

# generate leaflet map
map_catchment_sum <- json_events_by_zipcode_catchment %>%
  leaflet() %>%
  addProviderTiles(providers$Stamen.TonerLite) %>%
  addPolygons(
    data = json_events_by_zipcode_catchment,
    fillColor = ~ pal(json_events_by_zipcode_catchment$sum_participants),
    fillOpacity = .8,
    popup = ~popup_sb,
    weight = 1,
    color = "white"
  ) %>%
  addLegend(
    pal = pal,
    values = json_events_by_zipcode_catchment$sum_participants,
    title = "Total participants"
  )

map_catchment_sum

# save to disk
mapshot(map_catchment_sum, file = "figures/maps/event_participants_catchment_choropleth.png")

# by county catchment only
# generate sf data for az counties
sf_az_counties <- counties(state = 04)

# combine json and stats
sf_events_by_county_catchment_stats <- geo_join(sf_az_counties,
  events_by_county_catchment_stats,
  by_sp = "NAMELSAD",
  by_df = "county",
  how = "inner"
)

# set bins
bins <- c(0, 40, 80, 150, 500, Inf)

# set palette
pal <- colorBin("YlGnBu",
  domain = sf_events_by_county_catchment_stats$sum_participants,
  bins = bins
)

# set popup
popup_sb <- paste0(
  "Participants: ",
  as.character(sf_events_by_county_catchment_stats$sum_participants)
)

# generate leaflet map
map_catchment_counties_sum <- sf_events_by_county_catchment_stats %>%
  leaflet() %>%
  addProviderTiles(providers$Stamen.TonerLite) %>%
  addPolygons(
    data = sf_events_by_county_catchment_stats,
    fillColor = ~ pal(sf_events_by_county_catchment_stats$sum_participants),
    fillOpacity = .8,
    popup = ~popup_sb,
    weight = 1,
    color = "white"
  ) %>%
  addLegend(
    pal = pal,
    values = sf_events_by_county_catchment_stats$sum_participants,
    title = "Total participants"
  )

map_catchment_counties_sum

# save to disk
mapshot(map_catchment_counties_sum, file = "figures/maps/event_participants_catchment_counties_choropleth.png")

# by county AZ state
# combine json and stats
sf_events_by_county_stats <- geo_join(sf_az_counties,
  events_by_county_stats,
  by_sp = "NAMELSAD",
  by_df = "county",
  how = "inner"
)

# set bins
bins <- c(0, 40, 80, 200, 800, Inf)

# set palette
pal <- colorBin("YlGnBu",
  domain = sf_events_by_county_stats$sum_participants,
  bins = bins
)

# set popup
popup_sb <- paste0(
  "Participants: ",
  as.character(sf_events_by_county_stats$sum_participants)
)

# generate leaflet map
map_state_counties_sum <- sf_events_by_county_stats %>%
  leaflet() %>%
  addProviderTiles(providers$Stamen.TonerLite) %>%
  addPolygons(
    data = sf_events_by_county_stats,
    fillColor = ~ pal(sf_events_by_county_stats$sum_participants),
    fillOpacity = .8,
    popup = ~popup_sb,
    weight = 1,
    color = "white"
  ) %>%
  addLegend(
    pal = pal,
    values = sf_events_by_county_stats$sum_participants,
    title = "Total participants"
  )

map_state_counties_sum

# save to disk
mapshot(map_state_counties_sum, file = "figures/maps/event_participants_state_counties_choropleth.png")

# printable maps ----
# data
sf_az_zipcodes <- geojson_sf("GIS/az_arizona_zip_codes_geo.min.json")

# entire state
# join sf and stats
sf_events_by_zipcode_stats <- geo_join(sf_az_zipcodes,
  events_by_zipcode_stats,
  by_sp = "ZCTA5CE10",
  by_df = "zipcode",
  how = "inner"
)

# generate choropleth and save
sf_events_by_zipcode_stats_map <- sf_events_by_zipcode_stats %>%
  mutate(fill_value = cut_number(sum_participants, n = 5)) %>%
  ggplot() +
  geom_sf(data = sf_az_counties) +
  geom_sf(mapping = aes(fill = fill_value, color = fill_value)) +
  theme_void() +
  labs(
    title = "Total Participants by Zip Code",
    subtitle = "UAZCC COE Outreach and Education Activities",
    caption = ""
  ) +
  scale_fill_brewer(
    aesthetics = c("color", "fill"),
    palette = "YlGnBu",
    name = "Participants",
    labels = c("<40", "<80", "<150", "<331", ">=331")
  )

sf_events_by_zipcode_stats %>%
  summarise(min_INTPTLAT10 = min(INTPTLAT10),
            max_INTPTLAT10 = max(INTPTLAT10))

# display
sf_events_by_zipcode_stats_map

# export to PNG
ggsave("sf_events_by_zipcode_stats_map.png",
  device = "png",
  path = "figures/maps/",
  dpi = 300,
  width = 8,
  units = "in",
  limitsize = TRUE
)

# catchment only
# join sf and stats
sf_events_by_zipcode_catchment_stats <- geo_join(sf_az_zipcodes,
  events_by_zipcode_catchment_stats,
  by_sp = "ZCTA5CE10",
  by_df = "zipcode",
  how = "inner"
)

sf_events_by_zipcode_catchment_stats %>% summarise(
  min_INTPTLON10 = min(INTPTLON10),
  max_INTPTLON10 = max(INTPTLON10)
)

# generate choropleth and save
sf_events_by_zipcode_catchment_stats_map <- sf_events_by_zipcode_catchment_stats %>%
  mutate(fill_value = cut_number(sum_participants, n = 5)) %>%
  ggplot() +
  geom_sf(data = sf_az_counties) +
  geom_sf(mapping = aes(fill = fill_value, color = fill_value)) +
  coord_sf(ylim = c(31, 33.4)) +
  theme_void() +
  labs(
    title = "Total Participants by Zip Code",
    subtitle = "UAZCC COE Outreach and Education Activities in the Catchment",
    caption = ""
  ) +
  scale_fill_brewer(
    aesthetics = c("color", "fill"),
    palette = "YlGnBu",
    name = "Participants",
    labels = c("<40", "<80", "<150", "<331", ">=331")
  )

# display
sf_events_by_zipcode_catchment_stats_map

# export to PNG
ggsave("sf_events_by_zipcode_catchment_stats_map.png",
  device = "png",
  path = "figures/maps/",
  dpi = 300,
  width = 8,
  units = "in",
  limitsize = TRUE
)

# combine map to show LTBC qualtrics registrations and event list choropleth
# load and select data
LTBC_qualtrics_registrations <- read_rds("data/tidy/ltbc_registration.rds") %>%
  select(zipcode) %>%
  drop_na()

# add mapping data
LTBC_qualtrics_registrations <- inner_join(zip_db, LTBC_qualtrics_registrations) %>%
  select(zipcode, county, state, lat, lng) %>%
  drop_na()

# generate map
# add points to existing choropleth
sf_events_by_zipcode_stats_map_LTBC <- sf_events_by_zipcode_stats_map +
  geom_jitter(data = LTBC_qualtrics_registrations, 
             mapping = aes(x = lng, y = lat),
             alpha = 0.3,
             color = "red",
             size = 1.5) +
  coord_sf(ylim = c(31, 37),
           xlim = c( -115, -108))

# view
sf_events_by_zipcode_stats_map_LTBC

# export to PNG
ggsave("sf_events_by_zipcode_stats_map_LTBC.png",
       device = "png",
       path = "figures/maps/",
       dpi = 300,
       width = 8,
       units = "in",
       limitsize = TRUE
)

# combine map to show MCC survey response
# load and select data
chat_response <- read_rds("data/tidy/COE_cat_chat.rds") %>%
  select(zipcode = zip_code) %>%
  drop_na()

# add mapping data
chat_response <- inner_join(zip_db, chat_response) %>%
  select(zipcode, county, state, lat, lng) %>%
  drop_na()

# generate map
# add points to existing choropleth
sf_events_by_zipcode_stats_map_LTBC_MCC <- sf_events_by_zipcode_stats_map_LTBC +
  geom_jitter(data = chat_response, 
              mapping = aes(x = lng, y = lat),
              alpha = 0.3,
              color = "red",
              size = 1.5) +
  guides(shape = guide_legend(chat_response), size = guide_legend(chat_response))
  

# view
sf_events_by_zipcode_stats_map_LTBC_MCC

# export to PNG
ggsave("sf_events_by_zipcode_stats_map_LTBC_MCC.png",
       device = "png",
       path = "figures/maps/",
       dpi = 300,
       width = 8,
       units = "in",
       limitsize = TRUE
)
