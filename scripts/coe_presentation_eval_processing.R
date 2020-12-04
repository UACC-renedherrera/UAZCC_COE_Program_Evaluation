# Presentation Evaluation

# set up ----
# load packages
library(here)
library(tidyverse)
library(lubridate)
library(ggthemes)

# read data set
presentations <- read_rds("data/tidy/COE_presentations_evaluation.rds")

# in order to display each question on one graph must first create tables
table_knowledge <- table(presentations$knowledge)
table_confidence <- table(presentations$confidence)
table_organization <- table(presentations$organization)
table_engagement <- table(presentations$engagement)
table_presentation <- table(presentations$presentation)

# bind altogether and select rows
assessment <- bind_rows(table_knowledge, table_confidence, table_organization, table_engagement, table_presentation) %>%
  mutate("key" = c("Knowledge", "Confidence", "Organization", "Engagement", "Presentation")) %>%
  select(key, Agree, Neutral, Disagree)

# make the data long
assessment <- assessment %>% 
  gather("Agree", "Neutral", "Disagree", key = "Response", value = "Value")

# visualize
ggplot(data = assessment, mapping = aes(x = key, y = Value, fill = Response)) +
  geom_col(position = "dodge", alpha = 0.8) +
  labs(title = "LTBC",
       subtitle = "Responses as of: 19 June 2020",
       y = "",
       x = "Question") +
  theme_solarized()

# visualize likely apply?
ggplot(data = presentations, mapping = aes(x = likely_apply)) +
  geom_bar(alpha = 0.8) +
  scale_x_discrete(drop = FALSE) +
  labs(title = "LTBC",
       subtitle = "Responses as of: 19 June 2020",
       y = "",
       x = "How likely are  you to apply what you learned today to your life?") +
  theme_solarized()

# show time stamp and language
presentations %>%
  select(survey_evaluation_timestamp, language) %>%
  arrange(survey_evaluation_timestamp)
