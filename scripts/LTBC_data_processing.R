# Data processing for LTBC survey from REDCap

# set up ----
# load packages
library(here)
library(tidyverse)
library(zipcodeR)
library(leaflet)
library(ggthemes)
library(mapview)

# Read data
LTBC_data <- read_rds("data/tidy/COE_presentations_evaluation.rds")

# email ---
# get email for newsletter
email_newsletter_list <- LTBC_data %>%
  filter(newsletter == "TRUE") %>%
  select(email, language, survey_evaluation_timestamp) %>%
  arrange(survey_evaluation_timestamp)

# get email for drawing
email_drawing_list <- LTBC_data %>%
  filter(drawing == "TRUE") %>%
  select(email, language, survey_evaluation_timestamp) %>%
  arrange(survey_evaluation_timestamp)

# summarize topics ----
# build a table with the results of
# Which topics were covered in the presentation?
topics <- LTBC_data %>%
  select(topic_cancer_education:topic_other_text)

topics <- rename(topics,
       "Cancer education" = "topic_cancer_education",
       "Research" = "topic_research",
       "Causes & prevention" = "topic_causes_prevention",
       "Survivorship" = "topic_survivorship",
       "Treatment" = "topic_treatment",
       "Policy" = "topic_policy",
       "Screening" = "topic_screening",
       "Clinical trials" = "topic_clinical_trials",
       "Health promotion" = "topic_health_promotion",
       "Other topics" = "topic_other",
       "Breast" = "topic_cancer_breast",
       "Colorectal" = "topic_cancer_colorectal",
       "Prostate" = "topic_cancer_prostate",
       "Cervical" = "topic_cancer_cervical",
       "Lung" = "topic_cancer_lung",
       "Liver" = "topic_cancer_liver",
       "Skin" = "topic_cancer_skin",
       "Other cancer" = "topic_cancer_other",
       "Other cancer text" = "topic_cancer_other_text",
       "Other topics text" = "topic_other_text")

str(topics)

topics_summarized <- summarise_all(topics, funs(mean)) %>%
  select(`Cancer education`:`Other topics`, `Other topics text`)

topics_summarized_plot <- topics_summarized %>%
  gather(key = "topic",
         value = "value") %>%
  drop_na() %>%
  arrange(value) %>%
  mutate(topic = factor(topic, levels = topic)) %>%
  ggplot(mapping = aes(x = topic, y = value)) +
  geom_col(fill = "#0C234B") +
  coord_flip() +
  ylim(min = 0, max = 1) +
  labs(title = "Topics",
       subtitle = "Which topics were covered in the presentation?",
                               y = "",
                               x = "",
       caption = "Source: UAZCC COE Presentation Evaluation") +
  theme_hc()

topics_summarized_table <- topics_summarized %>%
  gather(key = "topic",
         value = "value") %>%
  drop_na() %>%
  arrange(desc(value))

# summarize cancer education ----
# build a table with the results of
# Please specific the cancer education provided?
topics_cancer_edu <- summarise_all(topics, funs(mean)) %>%
  select(`Breast`:`Other cancer text`)

topics_cancer_edu_plot <- topics_cancer_edu %>%
  gather(key = "topic",
         value = "value") %>%
  drop_na() %>%
  arrange(value) %>%
  mutate(topic = factor(topic, levels = topic)) %>%
  ggplot(mapping = aes(x = topic, y = value)) +
  geom_col(fill = "#0C234B") +
  coord_flip() +
  ylim(min = 0, max = 1) +
  labs(title = "Topics: Cancer Education",
       subtitle = "Please specify the cancer education provided?",
       y = "",
       x = "",
       caption = "Source: UAZCC COE Presentation Evaluation") +
  theme_hc()

topics_cancer_edu_table <- topics_cancer_edu %>%
  gather(key = "topic",
         value = "value") %>%
  drop_na() %>%
  arrange(desc(value))

# summarize organization affiliation ----
# build a table with the results of
# Which organizations were represented by the audience?
orgs <- LTBC_data %>%
  select(org_academia:org_self_describe_text)

str(orgs)

orgs <- rename(orgs,
                 "Academia" = "org_academia",
                 "Government" = "org_government",
                 "Health care" = "org_health_care",
                 "Community" = "org_community",
                 "Other" = "org_self_describe",
                 "Prefer not to say" = "org_prefer_not_to_say",
                 "Other text" = "org_self_describe_text")

orgs_summarized <- summarise_all(orgs, funs(mean))

orgs_summarized_plot <- orgs_summarized %>%
  gather(key = "org",
         value = "value") %>%
  drop_na() %>%
  arrange(value) %>%
  mutate(org = factor(org, levels = org)) %>%
  ggplot(mapping = aes(x = org, y = value)) +
  geom_col(fill = "#0C234B") +
  coord_flip() +
  ylim(min = 0, max = 1) +
  labs(title = "Organization",
       subtitle = "What is your organization affiliation?",
       y = "",
       x = "",
       caption = "Source: UAZCC COE Presentation Evaluation") +
  theme_hc()

orgs_summarized_table <- orgs_summarized %>%
  gather(key = "org",
         value = "value") %>%
  drop_na() %>%
  arrange(desc(value))

# mapping ----
# save zip codes
# save usa zip code to environment
zip_db <- zip_code_db

# save zip codes
zip_code_list <- as_tibble(LTBC_data$zip_code)

zip_code_list <- rename(zip_code_list, "zipcode" = "value")

distinct(zip_code_list) %>%
  arrange(zipcode)

# match up zip codes from eval with usa database
zip_code_map <- inner_join(zip_code_list, zip_db, by = "zipcode") %>%
  drop_na()

# zip code list with county
zip_code_list <- zip_code_map %>%
  select(zipcode, county, state) %>%
  count(county) %>%
  arrange(desc(n))

# generate map
mapped_zipcodes <- leaflet(data = zip_code_map) %>%
  addTiles() %>%
  addMarkers(~lng, ~lat)

mapped_zipcodes

# print map with mapview
mapshot(mapped_zipcodes, file = "figures/LTBC_mapped_zip_codes.png")

# charts & tables ----
# language
language_plot <- LTBC_data %>%
  ggplot(mapping = aes(x = language)) +
  geom_bar(fill = "#0C234B", stat = "count") +
  labs(title = "Language Preference",
       subtitle = "As indicated on survey",
       y = "",
       x = "",
       caption = "Source: UAZCC COE Presentation Evaluation") +
  theme_hc()

language_table <- LTBC_data %>%
  count(language)

# gender
gender_plot <- LTBC_data %>%
  ggplot(mapping = aes(x = gender)) +
  geom_bar(fill = "#0C234B", stat = "count") +
  labs(title = "Gender",
       y = "",
       x = "",
       caption = "Source: UAZCC COE Presentation Evaluation") +
  theme_hc()

gender_table <- LTBC_data %>%
  count(gender)

# age
age_plot <- LTBC_data %>%
  ggplot(mapping = aes(x = age)) +
  geom_histogram(fill = "#0C234B", bins = 10) +
  labs(title = "Age",
       y = "",
       x = "",
       caption = "Source: UAZCC COE Presentation Evaluation") +
  xlim(min = 0, max = 100) +
  ylim(min = 0, max = 5) +
  theme_hc()

age_table_list <- LTBC_data %>%
  select(age) %>%
  drop_na() %>%
  arrange(age)

age_table <- LTBC_data %>%
  select(age) %>%
  drop_na() %>%
  arrange(age) %>%
  summarize(n(),
            min(age),
            mean(age),
            median(age),
            max(age)) %>%
  gather(key = "statistic",
         value = "value")

# race
race_plot <- LTBC_data %>%
  ggplot(mapping = aes(x = race)) +
  geom_bar(fill = "#0C234B", stat = "count") +
  labs(title = "Race and Ethnicity",
       y = "",
       x = "",
       caption = "Source: UAZCC COE Presentation Evaluation") +
  theme_hc()

race_table <- LTBC_data %>%
  count(race) %>%
  arrange(desc(n))

# feedback ----
feedback <- select(LTBC_data, knowledge:presentation) %>%
  select(-(marketing))

feedback <- feedback %>%
  gather(key = "metric",
         value = "value") %>%
  mutate(value = factor(value, levels = c("Disagree",
                                          "Neutral",
                                          "Agree"), ordered = TRUE)) %>%
  mutate(metric = factor(metric))

feedback_table <- feedback %>%
  group_by(metric) %>%
  count(value)

feedback$metric <- recode_factor(feedback$metric,
                                 "knowledge" = "This presentation has \nincreased my knowledge of this topic?",
                                 "confidence" = "I can have conversations with others about this topic.",
                                 "organization" = "The presentation was: \nOrganized and Easy to Follow",
                                 "engagement" = "The presentation: \nEngaged the Audience",
                                 "presentation" = "Presenter(s) Spoke Clearly")

ggplot(data = feedback, mapping = aes(x = metric, fill = value)) +
  geom_bar(position = "fill", color = "white") +
  coord_flip() +
  labs(title = "Feedback",
       subtitle = "How much do you agree or disagree with the following?",
       y = "",
       x = "",
       caption = "Source: UAZCC COE Presentation Evaluation") +
  theme_hc()

# likely apply
applied_plot <- LTBC_data %>%
  ggplot(mapping = aes(x = likely_apply)) +
  geom_bar(fill = "#0C234B", stat = "count") +
  labs(title = "Real world application",
       subtitle = "How likely are you to apply what you learned today to your life?",
       y = "",
       x = "",
       caption = "Source: UAZCC COE Presentation Evaluation") +
  theme_hc()

applied_table <- LTBC_data %>%
  count(likely_apply)

# newsletter
newsletter_plot <- LTBC_data %>%
  ggplot(mapping = aes(x = newsletter)) +
  geom_bar(fill = "#0C234B", stat = "count") +
  labs(title = "Newsletter sign up",
       y = "",
       x = "",
       subtitle = "Would you like to subscribe to our monthly newsletter?",
       caption = "Source: UAZCC COE Presentation Evaluation") +
  theme_hc()

LTBC_data %>%
  count(newsletter) %>%
  arrange(desc(n))

# drawing
drawing_plot <- LTBC_data %>%
  ggplot(mapping = aes(x = drawing)) +
  geom_bar(fill = "#0C234B", stat = "count") +
  labs(title = "Drawing Entry",
       y = "",
       x = "",
       subtitle = "Would you like to enter a drawing to receive a giveaway?",
       caption = "Source: UAZCC COE Presentation Evaluation") +
  theme_hc()

drawing_table <- LTBC_data %>%
  count(drawing) %>%
  arrange(desc(n))

# marketing
marketing <- LTBC_data %>%
  select(marketing) %>%
  drop_na() %>%
  arrange(marketing)

# comments
# missing
# Did we miss anything important in this presentation that should be included next time?
LTBC_data %>%
  select(comments_missing) %>%
  drop_na() %>%
  arrange(comments_missing)

# questions
# What questions do you have about the presentation?
comments_missed <- LTBC_data %>%
  select(comments_questions) %>%
  drop_na() %>%
  arrange(comments_questions)

# other comments
# Comments, recommendations, or suggestions for future topics.
comments_questions <- LTBC_data %>%
  select(comments_other) %>%
  drop_na() %>%
  arrange(comments_other)
