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


# Clean Data
symptoms_ind <- function(symptom, fever) {  ## creating a function for standardizing the symptoms
  case_when(fever == "undisclosed" | fever == "under investigation" ~ NA_character_,
            symptom == "x" ~ "Yes",
            symptom == "" ~ "No")
  }

cases_reddit <- cases_reddit %>%  
  mutate_at(vars(names(cases_reddit)), na_if, "-") %>% ## replace "-", "?","For Validation", and blanks with NA
  mutate_at(vars(names(cases_reddit)), na_if, "?") %>% 
  mutate_at(vars(names(cases_reddit)), na_if, "For Validation") %>% 
  separate(status1, c("status", "status_date"), sep = "[()]") %>% ## split status variables
  mutate(travel = as.factor(travel), ## make variables factors
         sex = as.factor(sex),
         nationality = as.factor(nationality),
         cough = symptoms_ind(cough, fever), ## format the symptoms
         sore_throat = symptoms_ind(sore_throat, fever),
         pneumonia = symptoms_ind(pneumonia, fever),
         status = if_else(status == "Died ", "Dead", trimws(status)), ## make the statuses uniform
         symptoms_others = if_else(fever == "asymptomatic (no symptoms)", "asymptomatic (no symptoms)", symptoms_others), ## noting the asymptomatic cases in symptoms_others instead
         fever = case_when(fever == "undisclosed" | fever == "under investigation" ~ NA_character_, 
                           fever == "asymptomatic (no symptoms)" | fever == "" ~ "No",
                           fever == "x" ~ "Yes"))
  

cases_reddit <- cases_reddit %>%  
  mutate_at(vars(names(cases_reddit)), na_if, "") %>% # remove blanks before formatting dates
  mutate(positive_date = mdy(str_c(positive_date, ", 2020")), ## format dates
         onset_date = mdy(str_c(onset_date, ", 2020")),
         status_date = mdy(str_c(status_date, ", 2020")))

