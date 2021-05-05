# set up
# packages
library(here)
library(tidyverse)
library(ggthemes) 


# data
az_counties <- read_rds("data/tidy/spatial_az_counties.rds")
event_summary <- read_rds("data/tidy/spatial_event_summary.rds")
virtual_participants <- read_rds("data/tidy/spatial_virtual_participants.rds")

glimpse(event_summary)

event_summary %>%
  drop_na(participants_per_event) %>%
  summarize(
    minimum = min(participants_per_event),
    sd = sd(participants_per_event),
    median = median(participants_per_event),
    maximum = max(participants_per_event)
  ) %>%
  knitr::kable()

event_summary %>%
  drop_na(participants_per_event) %>%
  mutate(log_participants = log10(participants_per_event)) %>%
  summarize(
    minimum = min(log_participants),
    sd = sd(log_participants),
    median = median(log_participants),
    maximum = max(log_participants)
  )

ggplot(data = event_summary, mapping = aes(x = participants_per_event)) +
  geom_histogram(bins = 30)

fivenum(event_summary$participants_per_event)

event_summary %>%
  drop_na(participants_per_event) %>%
#   mutate(log_participants = log10(participants_per_event)) %>%
  ggplot() +
  geom_sf(data = az_counties, fill = "#F4EDE5") +
  geom_sf(mapping = aes(geometry = geometry, fill = participants_per_event)) +
  geom_sf(data = virtual_participants, color = "#A95C42", alpha = 0.4, size = 3) +
  scale_fill_viridis_c() +
  theme_void() +
  labs(title = "Statewide COE Outreach Events")
