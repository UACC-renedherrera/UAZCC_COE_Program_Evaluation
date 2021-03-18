# introduction
# the purpose of this script is to

# setup ----
# packages
library(here)
library(tidyverse)
library(qualtRics)
library(lubridate)

# load list of surveys from qualtrics
surveys <- all_surveys()
# display
surveys

# select naca fun run
funrun <- fetch_survey(surveys$id[4])
glimpse(funrun)
funrun <- funrun %>%
  arrange(desc(StartDate)) %>%
  select(#StartDate, 
         #RecipientFirstName, 
         #RecipientLastName, 
         RecipientEmail)

# read excel file of eventbrite 
eb <- read_csv("data/raw/eventbrite_Orders-140215196427.csv")

# change date from character to date and sort newest to oldest
eb <- eb %>%
  mutate("Date" = mdy(Date)) %>%
  arrange(desc(Date))

# select only email
eb <- eb %>%
  select(#Date, 
         #`First Name`, 
         #`Last Name`, 
         Email)

# inspect
glimpse(eb)

# find differences
difs <- setdiff(funrun$RecipientEmail, eb$Email)

# find matches 
matches <- semi_join(funrun, eb, by = c("RecipientEmail" = "Email")) %>%
  arrange(RecipientEmail)

# dave to disk 
write_csv(matches, "data/raw/email_matches.csv")

# find differences 
clashes <- anti_join(funrun, eb, by = c("RecipientEmail" = "Email")) %>%
  arrange(RecipientEmail)

# save to disk 
write_csv(clashes, "data/raw/email_clashes.csv")
