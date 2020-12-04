# set up ----
# load packages
library(here)
library(tidyverse)
library(readxl)
library(AnthroTools)

# read dataset to environment
chat_data <- read_rds("data/tidy/COE_cat_chat.rds")

# select variables of interest
# save to environment
chat_text_data <- chat_data %>% 
  select(record_id,
    "1" = uazcc_priority_1,
         "2" = uazcc_priority_2,
         "3" = uazcc_priority_3) %>%
mutate(record_id = as.numeric(record_id))

# arrange data for coding
chat_text_data <- chat_text_data %>%
  gather(`1`:`3`, key = "priority",
         value = "description") %>%
  arrange(record_id, priority) %>%
  drop_na()

# write to plain text file for analysis in MAXQDA 
write_delim(chat_text_data, "data/tidy/cat_chat_text.txt", delim = ",", na = "NA", col_names = TRUE)

# import xlsx of coded file 
chat_text_data_coded <- read_xlsx("data/raw/cat_chat_coded_segments.xlsx")

# basic frequency of coded themes
cat_chat_text_themes <- chat_text_data_coded %>%
  select(Code, Segment) %>%
  arrange(Code, Segment) %>%
  group_by(Code) %>%
  count() %>%
  arrange(desc(n))

cat_chat_text_themes

# read code by response 
cat_chat_text_for_salience <- read_csv("data/raw/cat_chat_text_coded.csv")

cat_chat_text_for_salience <- drop_na(cat_chat_text_for_salience)

cat_chat_text_for_salience <- as.data.frame(cat_chat_text_for_salience)

cat_chat_salience <- CalculateSalience(cat_chat_text_for_salience,
                  Order = "priority",
                  Subj = "record_id",
                  CODE = "code",
                  Rescale = TRUE)

cat_chat_salience

cat_chat_salience_by_code <- SalienceByCode(cat_chat_salience,
               CODE = "code",
               Salience = "Salience",
               Subj = "record_id",
               dealWithDoubles = "SUM")

cat_chat_salience_by_code <- as_tibble(cat_chat_salience_by_code)

cat_chat_salience_by_code %>%
  arrange(desc(MeanSalience), desc(SumSalience))
