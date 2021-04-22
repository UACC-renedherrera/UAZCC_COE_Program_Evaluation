# Data tidy for
# Beyond Cancer Event Registration
# and Survey 

# set up
# load package libraries 
library(here)
library(tidyverse)
library(ggthemes)
library(qualtRics)
library(zipcodeR)
library(sf)
library(tigris)
library(geojson)
library(knitr)
library(lubridate)
library(zipcodeR)

options(tigris_use_cache = TRUE)

# get az county spatial data
az_counties <- counties(state = 04)

# get az zip code spatial data 
zip_db <- zip_code_db

# collect data from qualtrics for survey
# load list of surveys
surveys <- all_surveys()
# display
surveys

# select zoom registration survey & inspect
beyond_registration <- fetch_survey(surveys$id[3])
glimpse(beyond_registration)

# select zoom registration survey & inspect
beyond_survey <- fetch_survey(surveys$id[7])
glimpse(beyond_survey)

# change character to date time 
beyond_survey$Q4.3 <- as_date(beyond_survey$Q4.3, format = "%m/%d/%Y")

beyond_survey <- beyond_survey %>%
  mutate(q4.3_year_dx = year(Q4.3)) 

# change zip code to character

beyond_registration <- beyond_registration %>%
  mutate(zipcode = as.character(Q1.3))

# list of email addresses registered
beyond_reg_email <- beyond_registration %>% 
  select(RecordedDate, Q1.2) %>%
  arrange(Q1.2) 

# beyond_reg_email <- str_to_lower(beyond_reg_email$Q1.2) %>%
#   as_tibble()

# save list to disk for calendar invitation   
write_csv(beyond_reg_email, file = "data/tidy/beyond_cancer_registration_email_list.csv")

# zip code location for beyond cancer registration 
registration_spatial <- inner_join(beyond_registration, zip_db, by = "zipcode") %>%
  filter(lat <= 37) %>%
  filter(lat >= 30) %>%
  filter(lng <= -109) %>%
  filter(lng >= -115) %>%
  select(lng, lat)

registration_spatial <- as.matrix(registration_spatial)
registration_spatial <- st_multipoint(x = registration_spatial)
registration_spatial <- st_sfc(registration_spatial, crs = 4269)

# map of IP address location for beyond cancer registration 
beyond_reg_mapping <- beyond_registration %>%
  select(lng = LocationLongitude,
         lat = LocationLatitude
         ) %>%
  filter(lat <= 37) %>%
  filter(lat >= 30) %>%
  filter(lng <= -109) %>%
  filter(lng >= -115)

beyond_reg_mapping <- as.matrix(beyond_reg_mapping)

beyond_reg_mapping <- st_multipoint(x = beyond_reg_mapping)

beyond_reg_mapping <- st_sfc(beyond_reg_mapping, crs = 4269)

ggplot() +
  geom_sf(data = az_counties) +
  geom_sf(data = beyond_reg_mapping, mapping = aes(color = "IP Address")) +
  geom_sf(data = registration_spatial, mapping = aes(color = "Zip Code")) +
  theme_map()

st_write(
  obj = az_counties,
  dsn = "data/spatial/tigris/az_counties/az_counties.shp",
  append = FALSE
)

st_write(
  obj = beyond_reg_mapping,
  dsn = "data/spatial/beyond_cancer_registrations/beyond_cancer_registrations.shp",
  append = FALSE
)

# survey responses 
glimpse(beyond_survey)

beyond_survey %>%
  select(starts_with("Q3.2")) %>%
  pivot_longer(
    cols = c(1,3,6,7),
    names_to = "name",
    values_to = "value"
  ) %>%
  drop_na(value) %>%
  count(value) %>%
  arrange(desc(n)) %>%
  kable(
    col.names = c("Impact statement", "Number"),
    caption = "How has cancer impacted your life?"
  )

beyond_survey %>%
  select(starts_with("Q3.3")) %>%
  pivot_longer(
    cols = c(1:5,7),
    names_to = "name",
    values_to = "value"
  ) %>%
  drop_na(value) %>%
  count(value) %>%
  arrange(desc(n)) %>%
  kable(
    col.names = c("Topic", "Number"),
    caption = "Which topics are you hoping to hear more about?"
  )

beyond_survey %>%
  select(starts_with("Q3.4")) %>%
  drop_na() %>%
  kable(
    col.names = c("Expectation response"),
    caption = "What are your expectations?"
  )

beyond_survey %>%
  select(starts_with("Q5.2")) %>%
  count(Q5.2) %>%
  arrange(desc(n)) %>%
  kable(
    col.names = c("Response", "Number"),
    caption = "Have you joined a support group?"
  )  

beyond_survey %>%
  select(starts_with("Q5.3")) %>%
  drop_na() %>%
  kable(
    col.names = c("Support Group"),
    caption = "What is the name of the support group?"
  )

beyond_survey %>%
  select(starts_with("Q5.4")) %>%
  drop_na() %>%
  kable(
    col.names = c("Source / Referral"),
    caption = "How did you find the support group?"
  )

beyond_survey %>%
  select(starts_with("Q4.6")) %>%
  drop_na() %>%
  kable(
    col.names = c("Resources"),
    caption = "What other survivorship resources have you accessed or found useful?"
  )

beyond_survey %>%
  select(starts_with("Q4.2")) %>%
  pivot_longer(
    cols = c(1,11),
    names_to = "name",
    values_to = "value"
  ) %>%
  drop_na(value) %>%
  count(value) %>%
  arrange(desc(n)) %>%
  kable(
    col.names = c("Cancer site", "Number"),
    caption = "Which cancer are you diagnosed with?"
  )

beyond_survey %>%
  select(q4.3_year_dx) %>%
  drop_na() %>%
  arrange(desc(q4.3_year_dx)) %>%
  kable(
    col.names = c("Year"),
    caption = "When were you first diagnosed with cancer?"
  )

beyond_survey %>%
  select(starts_with("Q4.4")) %>%
  pivot_longer(
    cols = c(1,4,5,6),
    names_to = "name",
    values_to = "value"
  ) %>%
  drop_na(value) %>%
  count(value) %>%
  arrange(desc(n)) %>%
  kable(
    col.names = c("Survivorship statement", "Number"),
    caption = "Where are you in cancer survivorship journey?"
  )

beyond_survey %>%
  select(starts_with("Q4.5")) %>%
  count(Q4.5) %>%
  arrange(desc(n)) %>%
  kable(
    col.names = c("Response", "Number"),
    caption = "Have you been given a survivorship care plan?"
  )  
