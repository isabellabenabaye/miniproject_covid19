library(tidyverse)
library(rvest)

# Preparing Data
## import data from the r/Coronavirus_PH Google sheet
url <- "https://docs.google.com/spreadsheets/d/1wdxIwD0b58znX4UrH6JJh_0IhnZP0YWn23Uqs7lHB6Q/gviz/tq?tqx=out:html&tq&gid=0"
page <- read_html(url) ## creates an html document from URL
table <- html_table(page, fill = TRUE) ## parses tables into data frames

cases_reddit <- as_tibble(table[[1]]) ## make the table
names(cases_reddit) <- c("case", "age", "sex", "nationality", "status1", "status2", "positive_date", "onset_date", "fever", "cough", "sore throat", "pneumonia", "symptoms_others", "other_conditions", "facility", "residence", "travel", "travel_history", "case_link", "link_desc", "details", "source", "column3", "column4", "column5", "column6", "column7", "column8", "column9", "column10") ## properly label the columns
cases_reddit <- cases_reddit[-1, 1:22] ## remove unnecessary rows/columns