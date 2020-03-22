library(tidyverse)
library(rvest)
library(lubridate)

# Preparing Data
## import data from the r/Coronavirus_PH Google sheet
url <- "https://docs.google.com/spreadsheets/d/1wdxIwD0b58znX4UrH6JJh_0IhnZP0YWn23Uqs7lHB6Q/gviz/tq?tqx=out:html&tq&gid=0"
page <- read_html(url) ## creates an html document from URL
table <- html_table(page, fill = TRUE) ## parses tables into data frames

cases_reddit <- as_tibble(table[[1]]) ## make the table
cases_reddit <- cases_reddit[-1, 1:22] ## remove unnecessary rows/columns
names(cases_reddit) <- c("case", "age", "sex", "nationality", "status1", "status_admitted", "positive_date", "onset_date", "fever", "cough", "sore_throat", "pneumonia", "symptoms_others", "other_conditions", "facility", "residence", "travel", "travel_history", "case_link", "link_desc", "details", "source") ## properly label the columns

## Fixing this mid-March shit
cases_reddit$status1 <- str_replace_all(cases_reddit$status1, "mid-March", "March 15") 

# Clean Data

symptoms_ind <- function(symptom) {  ## creating a function for standardizing the symptoms
  case_when(cases_reddit$fever == "undisclosed" | cases_reddit$fever == "under investigation" ~ NA_character_,
            symptom == "x" ~ "Yes",
            symptom == "" | symptom == "asymptomatic (no symptoms)" ~ "No",
            TRUE ~ symptom) ## catch other values
} ## I used this function below
others <- cases_reddit$symptoms_others %>% strsplit(", ") %>% unlist() %>% unique() ## list other symptoms

cases_reddit <- cases_reddit %>%  
  
  ## Making other_symptoms have their own column
  separate(symptoms_others, c('symptoms_others1', 'symptoms_others2'), sep = ", ") %>%
  mutate(others1 = ifelse(symptoms_others1 != "", "Yes", "No"),
         others2 = ifelse(symptoms_others1 != "", "Yes", "No")) %>% ## make the symptoms_others indicators uniform
  spread(symptoms_others1, others1, fill = "No") %>% 
  spread(symptoms_others2, others2, fill = "No") %>%

  ## Fixing main symptoms columns
  mutate_all(function (x) ifelse(x %in% c("-", "?", "For Validation"), NA, x)) %>% ## replace "-", "?","For Validation", and blanks with NA
  mutate(asymptomatic = case_when(fever == "asymptomatic (no symptoms)" ~ "Yes",   ## add indicator for asymptotic cases
                                  cases_reddit$fever == "undisclosed" | cases_reddit$fever == "under investigation" ~ NA_character_,
                                  TRUE ~ "No")) %>%
  mutate_at(vars(cough, sore_throat, pneumonia, fever, others), symptoms_ind) %>%  ## apply symptoms_ind fn to the main symptoms
  
  ## Fixing the status column
  separate(status1, c("status", "status_end_date"), sep = "[()]") %>% ## split status variables
  mutate(status = ifelse(status == "Died ", "Dead", trimws(status))) %>% ## Make status uniform
  
  ## Finalizing and formatting
  mutate_all(na_if, "") %>% ## Make all blanks NA
  mutate(positive_date = str_c(positive_date, ", 2020"), ## format dates
         onset_date = str_c(onset_date, ", 2020"),
         status_end_date = str_c(status_end_date, ", 2020")) %>%
  mutate_at(vars(age), as.integer) %>% ## changing types
  mutate_at(vars(sex, nationality, status, 
                 fever, cough, sore_throat, pneumonia, others,
                 travel), as.factor) %>%
  mutate_at(vars(contains("date")), mdy) %>%
  select(case, age, sex, nationality, ## Arranging columns
         status, status_end_date, status_admitted, 
         positive_date, onset_date,
         fever, cough, sore_throat, pneumonia,
         others, asymptomatic,
         other_conditions,
         facility, residence,
         travel, travel_history,
         case_link, link_desc) 

