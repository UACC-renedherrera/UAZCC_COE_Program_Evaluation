# Presentation Evaluation

# set up ----
# load packages
library(here)
library(tidyverse)
# library(dataMaid)
library(zipcodeR)
library(leaflet)
library(mapview)
library(lubridate)
library(ggthemes)

# read data from csv
chat_data <- read_csv("data/raw/EvaluationForMorning_DATA_2020-11-02_0910.csv",
                      skip = 1,
  col_names = c(
    "record_id", 
    "redcap_survey_identifier", 
    "survey_evaluation_timestamp", 
    "survey_text_evaluation_survey", 
    "language",
    "gender", 
    "gender_other_text", 
    "age", 
    "race", 
    "race_other_text",
    "org_academia", 
    "org_government", 
    "org_health_care", 
    "org_community", 
    "org_other",
    "org_na", 
    "org_other_text", 
    "zip_code", 
    "scientist_understand", 
    "scientist_steps",
    "learning_research", 
    "learning_beneficial", 
    "learning_contribute", 
    "learning_familiar", 
    "uazcc_effective",
    "uazcc_effective_other_text", 
    "uazcc_priority_1", 
    "uazcc_priority_2", 
    "uazcc_priority_3", 
    "recommend",
    "recommend_family", 
    "recommend_colleague", 
    "recommend_friend", 
    "recommend_other", 
    "recommend_other_text",
    "cancer_ed_method", 
    "cancer_ed_method_other", 
    "newsletter", 
    "email", 
    "evaluation_survey_complete"
  ),
  col_types = cols(
    "record_id" = col_character(), 
    "redcap_survey_identifier" = col_character(), 
    "survey_evaluation_timestamp" = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
    "survey_text_evaluation_survey" = col_skip(), 
    "language" = col_factor(levels = c(1:2)),
    "gender" = col_factor(levels = c(1:6)), 
    "gender_other_text" = col_character(), 
    "age" = col_number(), 
    "race" = col_factor(levels = c(1:7)), 
    "race_other_text" = col_character(), 
    "org_academia" = col_logical(), 
    "org_government" = col_logical(), 
    "org_health_care" = col_logical(), 
    "org_community" = col_logical(), 
    "org_other" = col_logical(), 
    "org_na" = col_logical(),  
    "org_other_text" = col_character(), 
    "zip_code" = col_character(), 
    "scientist_understand" = col_factor(levels = c(0:5), ordered = TRUE), 
    "scientist_steps" = col_factor(levels = c(0:5), ordered = TRUE), 
    "learning_research" = col_factor(levels = c(0:5), ordered = TRUE),  
    "learning_beneficial" = col_factor(levels = c(0:5), ordered = TRUE), 
    "learning_contribute" = col_factor(levels = c(0:5), ordered = TRUE), 
    "learning_familiar" = col_factor(levels = c(0:5), ordered = TRUE), 
    "uazcc_effective" = col_factor(levels = c(1:3)), 
    "uazcc_effective_other_text" = col_character(), 
    "uazcc_priority_1" = col_character(), 
    "uazcc_priority_2" = col_character(),  
    "uazcc_priority_3" = col_character(), 
    "recommend" = col_factor(levels = c(1:2)),
    "recommend_family" = col_logical(), 
    "recommend_colleague" = col_logical(), 
    "recommend_friend" = col_logical(), 
    "recommend_other" = col_logical(), 
    "recommend_other_text" = col_character(),
    "cancer_ed_method" = col_factor(levels = c(0:7)),  
    "cancer_ed_method_other" = col_character(), 
    "newsletter" = col_factor(levels = c(1:2)), 
    "email" = col_character(), 
    "evaluation_survey_complete" = col_factor(levels = c(0:2))
))

# inspect
glimpse(chat_data)
names(chat_data)

# recode
chat_data$gender <- recode_factor(chat_data$gender,
  "1" = "Female",
  "2" = "Male",
  "3" = "Non-binary/ third gender",
  "4" = "Transgender",
  "5" = "Prefer to self-describe",
  "6" = "Prefer not to say"
)
chat_data$race <- recode_factor(chat_data$race,
  "1" = "White",
  "2" = "Black",
  "3" = "American Indian / Alaska Native",
  "4" = "Asian / Pacific Islander",
  "5" = "Hispanic",
  "6" = "Prefer to self-describe",
  "7" = "Prefer not to say"
)

# function to assign agreement levels
recode_agreement <- function(x){
  x <- recode_factor(x,
                                                  "1" = "Strongly Disagree",
                                                  "2" = "Disagree",
                                                  "3" = "Neutral",
                                                  "4" = "Agree",
                                                  "5" = "Strongly Agree",
                                                  "0" = "NA"
  )
  print(x)
}

chat_data$scientist_understand <- recode_agreement(chat_data$scientist_understand)
chat_data$scientist_steps <- recode_agreement(chat_data$scientist_steps)
chat_data$learning_research <- recode_agreement(chat_data$learning_research)
chat_data$learning_beneficial <- recode_agreement(chat_data$learning_beneficial)
chat_data$learning_contribute <- recode_agreement(chat_data$learning_contribute)
chat_data$learning_familiar <- recode_agreement(chat_data$learning_familiar)

chat_data$cancer_ed_method <- recode_factor(chat_data$cancer_ed_method,
  "1" = "Radio",
  "2" = "Newspaper",
  "3" = "Magazine",
  "4" = "TV",
  "5" = "Social media",
  "6" = "Email",
  "7" = "Other",
  "0" = "NA"
)
chat_data$newsletter <- recode_factor(chat_data$newsletter,
  "1" = "Yes",
  "2" = "No"
)
chat_data$uazcc_effective <- recode_factor(chat_data$uazcc_effective,
  "1" = "Yes",
  "2" = "No",
  "3" = "Other"
)
chat_data$language <- recode_factor(chat_data$language,
                                           "1" = "English",
                                           "2" = "Spanish")
chat_data$recommend <- recode_factor(chat_data$recommend,
                                    "1" = "1",
                                    "2" = "0")
chat_data$recommend <- as.numeric(chat_data$recommend)
chat_data$recommend <- as.logical(chat_data$recommend)
chat_data$evaluation_survey_complete <- recode_factor(chat_data$evaluation_survey_complete,
                                     "1" = "Unverified",
                                     "2" = "Complete",
                                     "0" = "Incomplete")


# inspect
glimpse(chat_data)

# save dataset ---- 
write_rds(chat_data, "data/tidy/COE_cat_chat.rds")

chat_data <- read_rds("data/tidy/COE_cat_chat.rds")

# email ---- 
# get email for newsletter 
chat_data_email <- chat_data %>%
  filter(newsletter == "Yes") %>%
  select(email, language, survey_evaluation_timestamp) %>%
  arrange(survey_evaluation_timestamp, email)

write_csv(chat_data_email, "data/tidy/cat_chat_email_list.csv")

# generate codebook
# makeCodebook(chat_data, replace = TRUE)

# Language ----
chat_data %>%
  ggplot(mapping = aes(x = language)) +
  geom_bar(stat = "count") +
  labs(title = "Language Preference",
       subtitle = "Response indicated on survey",
       y = "",
       x = "",
       caption = "Source: REDCap Evaluation for Morning Cat Chat") +
  theme_hc()

chat_data %>%
  count(language)

# Gender ----
chat_data %>%
  ggplot(mapping = aes(x = gender)) +
  geom_bar(stat = "count") +
  labs(title = "Gender",
       subtitle = "What is your gender?",
       y = "",
       x = "",
       caption = "Source: REDCap Evaluation for Morning Cat Chat") +
  theme_hc()

chat_data %>%
  count(gender)

# Age ----
chat_data %>%
  ggplot(mapping = aes(x = age)) +
  geom_histogram(bins = 10) +
  labs(title = "Age",
       subtitle = "What is your age, in years?",
       y = "",
       x = "",
       caption = "Source: REDCap Evaluation for Morning Cat Chat") +
  xlim(min = 0, max = 100) +
  ylim(min = 0, max = 5) +
  theme_hc()

chat_data %>%
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

# Race ----
chat_data %>%
  ggplot(mapping = aes(x = race)) +
  geom_bar(stat = "count") +
  labs(title = "Race and Ethnicity",
       subtitle = "What is your race and ethnicity?",
       y = "",
       x = "",
       caption = "Source: REDCap Evaluation for Morning Cat Chat") +
  theme_hc()

chat_data %>%
  count(race)

# zip code map ----
# save zip codes 
# save usa zip code to environment
zip_db <- zip_code_db

# save zip codes
zip_code_list <- as_tibble(chat_data$zip_code)

zip_code_list <- rename(zip_code_list, "zipcode" = "value")

zip_code_list %>%
  count(zipcode) %>%
  arrange(desc(n))

distinct(zip_code_list) %>%
  arrange(zipcode)

# match up zip codes from eval with usa database
zip_code_map <- inner_join(zip_code_list, zip_db, by = "zipcode") %>%
  drop_na()

# generate map
mapped_zipcodes <- leaflet(data = zip_code_map) %>%
  addTiles() %>%
  addMarkers(~lng, ~lat)

mapped_zipcodes

# print map with mapview
mapshot(mapped_zipcodes, file = "figures/cat_chat_mapped_zip_codes.png")

# organization affiliation ----
# build a table with the results of
# Which organizations were represented by the audience?
orgs <- chat_data %>%
  select(org_academia:org_other_text)

str(orgs)

orgs_summarized <- summarise_all(orgs, funs(mean))

orgs_summarized <- rename(orgs_summarized, 
                          "academia" = "org_academia",
                          "government" = "org_government",
                          "health care" = "org_health_care",
                          "community" = "org_community",
                          "other" = "org_other", 
                          "NA" = "org_na")

orgs_summarized %>%
  gather(key = "org",
         value = "value") %>%
  drop_na() %>%
  arrange(value) %>%
  mutate(org = factor(org, levels = org)) %>%
  ggplot(mapping = aes(x = org, y = value)) +
  geom_col() +
  coord_flip() +
  ylim(min = 0, max = 1) + 
  labs(title = "Organization",
       subtitle = "What is your organization affiliation?",
       y = "",
       x = "",
       caption = "Source: REDCap Evaluation for Morning Cat Chat") +
  theme_hc()

orgs_summarized %>%
  gather(key = "org",
         value = "value") %>%
  drop_na() %>%
  arrange(desc(value))

orgs %>%
  select(org_other_text) %>%
  drop_na()

# presentation feedback ----
feedback <- chat_data %>%
  select(scientist_understand:learning_familiar)

str(feedback)

feedback <- feedback %>%
  gather(key = "metric",
         value = "value") %>%
  mutate(value = factor(value, levels = c("Strongly Disagree", 
  "Disagree", 
  "Neutral", 
  "Agree", 
  "Strongly Agree"), ordered = TRUE)) %>%
  mutate(metric = factor(metric))

feedback %>%
  group_by(metric) %>%
  count(value)

feedback$metric <- recode_factor(feedback$metric,
                                 "scientist_understand" = "The scientist explained their research project\n in a way that I can understand.",
                                 "scientist_steps" = "I learned what steps I should take\n to be a cancer research scientist.",
                                 "learning_research" = "I understand what this\n cancer research project is about.",
                                 "learning_beneficial" = "This cancer research project is\n beneficial for my community.",
                                 "learning_contribute" = "This presentation has contributed to\n my current knowledge of cancer research.",
                                 "learning_familiar" = "I was familiar with research projects conducted\n at the UAZCC before this presentation."
                                 )

ggplot(data = feedback, mapping = aes(x = metric, fill = value)) +
  geom_bar(position = "fill", color = "white") +
  coord_flip() +
  labs(title = "Feedback",
       subtitle = "How much do you agree or disagree with the following?",
       y = "",
       x = "",
       caption = "Source: REDCap Evaluation for Morning Cat Chat") +
  theme_hc()

# UAZCC effectiveness
uazcc <- chat_data %>%
  select(uazcc_effective:uazcc_priority_3)

uazcc %>%
  count(uazcc_effective) %>%
  ggplot(mapping = aes(x = uazcc_effective, y = n)) +
  geom_bar(stat = "identity") +
  labs(title = "Opinion of UAZCC",
       subtitle = "In the past, has the UAZCC been\n effective in cancer awareness and education in my community?",
       y = "",
       x = "",
       caption = "Source: REDCap Evaluation for Morning Cat Chat") +
  theme_hc()

uazcc %>%
  count(uazcc_effective)

uazcc %>%
  select(uazcc_effective_other_text) %>%
  drop_na()

uazcc %>%
  select(uazcc_priority_1:uazcc_priority_3) %>%
  arrange(uazcc_priority_1, uazcc_priority_2)

# recommendation?
recommendations <- chat_data %>%
  select(recommend:recommend_other_text)

recommendations

rec_summarized <- summarise_all(recommendations, funs(mean))

rec_summarized

rec_summarized %>%
  gather(key = "recommendation",
         value = "value") %>%
  drop_na() %>%
  arrange(value) %>%
  mutate(recommendation = factor(recommendation, levels = recommendation)) %>%
  ggplot(mapping = aes(x = recommendation, y = value)) +
  geom_col() +
  coord_flip() +
  ylim(min = 0, max = 1) + 
  labs(title = "Recommendation",
       subtitle = "If would you recommend this series to someone else \nthen who would you recommend this series to?",
       y = "",
       x = "",
       caption = "Source: REDCap Evaluation for Morning Cat Chat") +
  theme_hc()

rec_summarized %>%
  gather(key = "recommendation",
         value = "value") %>%
  drop_na() %>%
  arrange(desc(value))

recommendations %>%
  select(recommend_other_text) %>%
  drop_na()

# cancer education method ----
edu <- chat_data %>%
  select(cancer_ed_method:cancer_ed_method_other)

str(edu)

edu %>%
  ggplot(mapping = aes(x = cancer_ed_method)) +
  geom_bar(stat = "count") +
  labs(title = "Prefered method of cancer education",
       subtitle = "What is the best method of \ngetting cancer education materials to you?",
       y = "",
       x = "",
       caption = "Source: REDCap Evaluation for Morning Cat Chat") +
  theme_hc()

edu %>%
  count(cancer_ed_method)

