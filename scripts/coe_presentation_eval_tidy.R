# Presentation Evaluation
# in REDCap: 
# 1. CSV Raw
# 2. export survey identifier field and timestamp

# set up ----
# load packages
library(here)
library(tidyverse)
library(zipcodeR)
library(leaflet)
library(lubridate)
library(ggthemes)
library(mapview)
library(janitor)

# read from csv to environment
LTBC_data <- read_csv("data/raw/UAZCCCOEPresentation_DATA_2021-04-15_1147.csv") %>%
  clean_names()
  
# print list of email
list_email <- LTBC_data %>%
  select(email) %>%
  drop_na() %>%
  arrange(email) 

# change to lowercase 
list_email <- str_to_lower(list_email$email) %>%
  as_tibble()

# remove duplicates and save to disk as CSV 
list_email %>%
  distinct() %>%
  write_csv(file = "data/tidy/LTBC_email_list.csv")
  
# read data from CSV ----
# email commented out; if your data has email remove the comment
LTBC_data <- read_csv("data/raw/UAZCCCOEPresentation_DATA_2021-04-15_1147.csv",
                      skip = 1,
                      na = c("", "NA", "None", "none", "N/A"),
                      col_names = c(
                        "participant_id",
                        "redcap_survey_identifier",
                        "survey_evaluation_timestamp",
                        "language",
                        "gender",
                        "gender_self_describe",
                        "age",
                        "race",
                        "race_self_describe",
                        "org_academia",
                        "org_government",
                        "org_health_care",
                        "org_community",
                        "org_self_describe",
                        "org_prefer_not_to_say",
                        "org_self_describe_text",
                        "zip_code",
                        "topic_cancer_education",
                        "topic_research",
                        "topic_causes_prevention",
                        "topic_survivorship",
                        "topic_treatment",
                        "topic_policy",
                        "topic_screening",
                        "topic_clinical_trials",
                        "topic_health_promotion",
                        "topic_other",
                        "topic_cancer_breast",
                        "topic_cancer_colorectal",
                        "topic_cancer_prostate",
                        "topic_cancer_cervical",
                        "topic_cancer_lung",
                        "topic_cancer_liver",
                        "topic_cancer_skin",
                        "topic_cancer_other",
                        "topic_cancer_other_text",
                        "topic_other_text",
                        "knowledge",
                        "confidence",
                        "marketing",
                        "organization",
                        "engagement",
                        "presentation",
                        "likely_apply",
                        "comments_missing",
                        "comments_questions",
                        "comments_other",
                        "newsletter",
                        "drawing",
                        "email",
                        "survey_evaluation_complete"
                      ),
                      col_types = cols(
                        "participant_id" = col_character(),
                        "redcap_survey_identifier" = col_character(),
                        "survey_evaluation_timestamp" = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                        "language" = col_factor(levels = c(1:2)),
                        "gender" = col_factor(levels = c(1:6)),
                        "gender_self_describe" = col_character(),
                        "age" = col_number(),
                        "race" = col_factor(levels = c(1:7)),
                        "race_self_describe" = col_character(),
                        "org_academia" = col_logical(),
                        "org_government" = col_logical(),
                        "org_health_care" = col_logical(),
                        "org_community" = col_logical(),
                        "org_self_describe" = col_logical(),
                        "org_prefer_not_to_say" = col_logical(),
                        "org_self_describe_text" = col_character(),
                        "zip_code" = col_character(),
                        "topic_cancer_education" = col_logical(),
                        "topic_research" = col_logical(),
                        "topic_causes_prevention" = col_logical(),
                        "topic_survivorship" = col_logical(),
                        "topic_treatment" = col_logical(),
                        "topic_policy" = col_logical(),
                        "topic_screening" = col_logical(),
                        "topic_clinical_trials" = col_logical(),
                        "topic_health_promotion" = col_logical(),
                        "topic_other" = col_logical(),
                        "topic_cancer_breast" = col_logical(),
                        "topic_cancer_colorectal" = col_logical(),
                        "topic_cancer_prostate" = col_logical(),
                        "topic_cancer_cervical" = col_logical(),
                        "topic_cancer_lung" = col_logical(),
                        "topic_cancer_liver" = col_logical(),
                        "topic_cancer_skin" = col_logical(),
                        "topic_cancer_other" = col_logical(),
                        "topic_cancer_other_text" = col_character(),
                        "topic_other_text" = col_character(),
                        "knowledge" = col_factor(levels = c(1:3)),
                        "confidence" = col_factor(levels = c(1:3)),
                        "marketing" = col_character(),
                        "organization" = col_factor(levels = c(1:3)),
                        "engagement" = col_factor(levels = c(1:3)),
                        "presentation" = col_factor(levels = c(1:3)),
                        "likely_apply" = col_factor(levels = c(1:5)),
                        "comments_missing" = col_character(),
                        "comments_questions" = col_character(),
                        "comments_other" = col_character(),
                        "newsletter" = col_logical(),
                        "drawing" = col_logical(),
                        "email" = col_character(),
                        "survey_evaluation_complete" = col_factor(levels = c(0:2))
                      ))

glimpse(LTBC_data)

# recode ----
LTBC_data$language <- recode_factor(LTBC_data$language,
                                    "1" = "English",
                                    "2" = "Spanish")
LTBC_data$gender <- recode_factor(LTBC_data$gender,
                                    "1" = "Female",
                                    "2" = "Male",
                                  "3" = "Non-binary third gender",
                                  "4" = "Transgender",
                                  "5" = "Prefer to self-describe",
                                  "6" = "Prefer not to say")
LTBC_data$race <- recode_factor(LTBC_data$race,
                                "1" = "White",
                                "2" = "Black",
                                "3" = "American Indian and Alaska Native",
                                "4" = "Asian / Pacific Islander",
                                "5" = "Hispanic",
                                "6" = "Prefer to self-describe",
                                "7" = "Prefer not to say")

# function to set levels for agree and disagree
set_levels_of_agreement <- function(x){
  x <- recode_factor(x,
                                       "1" = "Disagree",
                                       "2" = "Neutral",
                                       "3" = "Agree")
  print(x)  
}

# recode 
LTBC_data$knowledge <- set_levels_of_agreement(LTBC_data$knowledge)
LTBC_data$confidence <- set_levels_of_agreement(LTBC_data$confidence)
LTBC_data$organization <- set_levels_of_agreement(LTBC_data$organization)
LTBC_data$engagement <- set_levels_of_agreement(LTBC_data$engagement)
LTBC_data$presentation <- set_levels_of_agreement(LTBC_data$presentation)
LTBC_data$likely_apply <- recode_factor(LTBC_data$likely_apply,
                                  "1" = "Very likely",
                                  "2" = "Likely",
                                  "3" = "Neutral",
                                  "4" = "Unlikely",
                                  "5" = "Very unlikely")
LTBC_data$survey_evaluation_complete <- recode_factor(LTBC_data$survey_evaluation_complete,
                                        "0" = "Incomplete",
                                        "1" = "Unverified",
                                        "2" = "Complete")

# inspect
glimpse(LTBC_data)

# save dataset ----
write_rds(LTBC_data, "data/tidy/COE_presentations_evaluation.rds")

# get email list 
LTBC_email_list <- LTBC_data %>%
  filter(drawing == TRUE) %>%
  select(survey_evaluation_timestamp, drawing, email) %>%
  arrange(desc(survey_evaluation_timestamp))

write_csv(LTBC_email_list, "data/tidy/LTBC_email_list.csv")
