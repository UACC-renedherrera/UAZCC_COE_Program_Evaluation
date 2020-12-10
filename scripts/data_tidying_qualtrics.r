# introduction
# the purpose of this script is to desribe the characteristics of UAZCC LTBC Qualtrics registrations by:
# 1. getting data from qualtrics 
# 2. tidy data
# 3. generate tables, charts, and maps 

# setup ----

# packages
library(tidyverse)
library(qualtRics)
library(zipcodeR)
library(leaflet)
library(ggthemes)

# load list of surveys
surveys <- all_surveys()
# display
surveys

# select zoom registration survey & inspect
zoom_reg <- fetch_survey(surveys$id[6])
glimpse(zoom_reg)

# exploratory data analysis ---- 
# which states?
distinct(zoom_reg, Q4_3)

# tidy data ----
table_zoom_reg <- zoom_reg %>%
  select(Q2_3, Q4_2, Q4_3, Q4_4, LocationLatitude, LocationLongitude) %>%
  arrange(Q2_3) %>%
  unique() %>%
  mutate(email = str_extract(Q2_3, "@.+"),
         zipcode = str_extract(Q4_2, "[\\d]{5}")) %>%
  select(email, 
         zipcode,
         state = Q4_3,
         org = Q4_4) 

table_zoom_reg$email <- str_to_lower(table_zoom_reg$email)
  
# inspect
glimpse(table_zoom_reg )

# save data to csv
write_csv(table_zoom_reg, "data/tidy/ltbc_registration.csv")

# save data to rds
write_rds(table_zoom_reg, "data/tidy/ltbc_registration.rds")

# prepare data for mapping ----
zoom_zips <- table_zoom_reg %>%
  count(zipcode) %>%
  arrange(desc(n)) %>%
  drop_na()

# print to console
zoom_zips

# save usa zip code to environment
zip_db <- zip_code_db

# match up zip codes from qualtrics with usa database
zoom_zips_map <- inner_join(zoom_zips, zip_db, by = "zipcode") %>%
  drop_na()

# inspect
glimpse(zoom_zips_map)

# generate map
leaflet(data = zoom_zips_map) %>%
  addTiles() %>%
  addMarkers(~lng, ~lat)

# data processing ---- 
# count unique email domain and save to csv
table_zoom_reg %>%
  count(email) %>%
  drop_na() %>%
  arrange(desc(n)) %>%
  write_csv("data/tidy/ltbc_registration_email.csv")

# plot chart of count of unique email domain
table_zoom_reg %>%
  count(email) %>%
  drop_na() %>%
  arrange(desc(n)) %>%
  ggplot(mapping = aes(x = n, y = reorder(email, n))) +
  geom_col() +
  labs(title = "email domain",
       x = "",
       y = "") +
  theme_classic()

# save plot of email to disk
ggsave(
  filename = "email.png",
  device = "png",
  path = "figures/",
  dpi = 100,
  limitsize = TRUE
)

# plot chart of count of unique zip code 
table_zoom_reg %>%
  count(zipcode) %>%
  drop_na() %>%
  arrange(desc(n)) %>%
  ggplot(mapping = aes(x = n, y = reorder(zipcode, n))) +
  geom_col() +
  labs(title = "Zip Code",
       x = "",
       y = "") +
  theme_classic()

# save plot of zipcode to disk 
ggsave(
  filename = "zipcode.png",
  device = "png",
  path = "figures/",
  dpi = 100,
  limitsize = TRUE
)

# count unique zipcode and save to csv 
table_zoom_reg %>%
  count(zipcode) %>%
  drop_na() %>%
  arrange(desc(n)) %>%
  write_csv("data/tidy/ltbc_registration_zipcode.csv")

# plot chart of count of unique state
table_zoom_reg %>%
  count(state) %>%
  drop_na() %>%
  arrange(desc(n)) %>%
  ggplot(mapping = aes(x = n, y = reorder(state, n))) +
  geom_col() +
  labs(title = "State",
       x = "",
       y = "") +
  theme_classic()

# save plot of state to disk 
ggsave(
  filename = "state.png",
  device = "png",
  path = "figures/",
  dpi = 100,
  limitsize = TRUE
)

# count unique zipcode and save to csv
table_zoom_reg %>%
  count(state) %>%
  drop_na() %>%
  arrange(desc(n)) %>%
  write_csv("data/tidy/ltbc_registration_state.csv")

