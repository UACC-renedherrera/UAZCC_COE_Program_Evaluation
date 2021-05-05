# packages
library(here)
library(tidyverse)
library(qualtRics)
library(zipcodeR)
# library(leaflet)
# library(ggthemes)
# library(tigris)
library(sp)
library(sf)

# # options
# options(tigris_use_cache = TRUE)
# 
# # load tigris az zip code boundaries 
# az_zip_codes <- zctas()

# load list of surveys
surveys <- all_surveys()

# display
surveys

# beyond cancer 
beyond_reg <- fetch_survey(surveys$id[3])

# inspect
glimpse(beyond_reg)

# select data for spatial analysis
beyond_reg <- beyond_reg %>%
  select(9, 14, 15)

# fun run 
naca_run <- fetch_survey(surveys$id[5])

# inspect
glimpse(naca_run)

# select data for spatial analysis
naca_run <- naca_run %>%
  select(9, 14, 15)

# ltbc registration zoom invite 
ltbc <- fetch_survey(surveys$id[9])

# inspect 
glimpse(ltbc)

# select data for spatial analysis
ltbc <- ltbc %>%
  select(9, 14, 15)

# join
virtual_participants <- bind_rows(
  ltbc, naca_run, beyond_reg
) %>%
  drop_na() %>%
  filter(LocationLongitude >= -114.8163,
         LocationLongitude <= -109.0452,
         LocationLatitude >= 31.33234,
         LocationLatitude <= 37.00372)

coordinates(virtual_participants) <- ~LocationLongitude + LocationLatitude

virtual_participants <- st_as_sf(virtual_participants)

virtual_participants <- st_set_crs(virtual_participants, 4269)
write_rds(virtual_participants, "data/tidy/spatial_virtual_participants.rds")

az_counties <- counties(state = "04")
write_rds(az_counties, "data/tidy/spatial_az_counties.rds")

st_write(virtual_participants,
         dsn = "data/spatial/virtual_participants/virtual_participants.shp",
         append = FALSE)

plot(virtual_participants)

ggplot() +
  geom_sf(data = az_counties) +
  geom_sf(data = virtual_participants)
