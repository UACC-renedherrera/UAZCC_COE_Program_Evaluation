# set up
# packages
library(here)
library(tidyverse)
library(ggthemes)
library(sf)

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
  drop_na(participants) %>%
  mutate(log_participants = log10(participants_per_event)) %>%
  ggplot() +
  geom_sf(data = az_counties, fill = "#E2E9EB") +
  geom_sf(mapping = aes(geometry = geometry, fill = log_participants)) +
  geom_sf(data = virtual_participants, color = "#AB0520", alpha = 0.5, size = 3, show.legend = TRUE) +
  scale_fill_distiller(
    palette = "YlGnBu",
    name = "log(Participants)"
  ) +
  theme_void() +
  labs(
    title = "Statewide COE Outreach Events",
    subtitle = "Logarithmic transformation of participants per event"
  )

event_summary %>%
  mutate(fill_value = cut_number(participants, n = 5)) %>%
  ggplot() +
  geom_sf(data = az_counties, 
          fill = "#E2E9EB", 
          alpha = 0.8) +
  geom_sf(mapping = aes(geometry = geometry, 
                        fill = fill_value, 
                        color = fill_value), 
          alpha = 0.8) +
  geom_sf(data = virtual_participants, 
          color = "#AB0520", 
          alpha = 0.3, 
          size = 3, 
          show.legend = FALSE) +
  scale_fill_brewer(
    aesthetics = c("color", "fill"),
    palette = "YlGnBu",
    name = "Participants",
    labels = c("<40", "<80", "<150", "<331", ">=331")
  ) +
  theme_void() +
  labs(
    title = "Statewide COE Outreach Events",
    subtitle = "Participants in Each Zip Code"
  ) +
  theme(
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    plot.title = element_text(size = 22),
    plot.subtitle = element_text(size = 18)
  )

event_summary %>%
  drop_na(participants) %>%
  # mutate(log_participants = log10(participants_per_event)) %>%
  ggplot() +
  geom_sf(data = az_counties, fill = "#E2E9EB") +
  geom_sf(mapping = aes(geometry = geometry, fill = participants)) +
  geom_sf(data = virtual_participants, color = "#AB0520", alpha = 0.5, size = 3, show.legend = TRUE) +
  ylim(32, 32.5) +
  xlim(-111.5, -110.5) +
  scale_fill_distiller(
    palette = "YlGnBu",
    name = "Participants"
  ) +
  theme_void() +
  labs(
    title = "Statewide COE Outreach Events",
    subtitle = "Tucson and Surrounding Area "
  )

event_summary %>%
  drop_na(participants) %>%
  # mutate(log_participants = log10(participants_per_event)) %>%
  ggplot() +
  geom_sf(data = az_counties, fill = "#E2E9EB") +
  geom_sf(mapping = aes(geometry = geometry, fill = participants)) +
  geom_sf(data = virtual_participants, color = "#AB0520", alpha = 0.5, size = 3, show.legend = TRUE) +
  ylim(33.1, 33.8) +
  xlim(-112.4, -111.4) +
  scale_fill_distiller(
    palette = "YlGnBu",
    name = "Participants"
  ) +
  theme_void() +
  labs(
    title = "Statewide COE Outreach Events",
    subtitle = "Phoenix and Surrounding Area "
  )
