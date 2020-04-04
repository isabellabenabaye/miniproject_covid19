library(tidyverse)
library(jsonlite)
library(lubridate)
# library(maps)

# Prepare Data
## import data from the official PH DOH tracker
json_file <- "https://services5.arcgis.com/mnYJ21GiFTR97WFg/arcgis/rest/services/PH_masterlist/FeatureServer/0/query?f=json&where=1%3D1&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=102100&cacheHint=true" ## url to the DOH tracker JSON file
doh_raw <- fromJSON(json_file)
cases_doh <- as_tibble(doh_raw$features$attributes) ## make the table
cases_doh <- cases_doh[, 2:16]
names(cases_doh) <- c("case", "caseID", "age", "sex", "nationality", "residence", "travel_history", "symptoms", "positive_date", "facility", "latitude", "longitude", "status", "link_desc", "updated") ## properly label the columns
raw_doh <- cases_doh


# Clean Data
cases_doh[1:3, "residence"] <- "China"

cases_doh <- raw_doh %>%  
  mutate_all(function (x) ifelse(x %in% c(" ","-", "?", "For Validation","For validation"), NA, x)) %>%  ## replace with NA
  mutate(residence = ifelse(str_detect(residence, ","), 
                            str_extract(residence, '\\b[^,]+$'),
                            residence)) %>%
  mutate_at(vars(names(cases_doh)), function(x) {str_replace_all(x, "�", "ñ")}) %>% 
  mutate_at(vars(facility), function(x) {x <- str_to_title(x) 
                                        x <- ifelse(na.fill(str_locate(x, "The")[,1] == 1, FALSE), x, str_replace(x, "The", "the"))
                                        x <- str_replace(x, "Of", "of")
                                        x <- str_replace(x, "And", "and")
                                        x <- str_replace(x, "For", "for")
                                        }) %>% 
  separate(updated, c("updated", "update_time"), sep = " ") #%>% 
  ## format columns
  mutate(case = as.character(case),
         positive_date = mdy(positive_date), ## format dates
         updated = mdy(updated),
         sex = as.factor(sex), ## factor sex and nationality
         nationality = as.factor(nationality),
         facility = as.factor(facility))

## Travel: "Yes" "No"
countries <- paste(world.cities$country.etc, collapse = "|")
cases_doh$travel <- ifelse(sapply(cases_doh$travel_history,
       function(x) {x %>%
                    str_replace_all("[()]|[;]", "") %>% ## replaces all paranthesis OR all ; with "" (deletes them)
                    str_detect(countries)}) == TRUE, "Yes", "No")

## Link: "1" "2"
cases_doh$link_desc <- str_extract_all(cases_doh$travel_history, '(?<=PH)[0-9|-]+') %>%  
                                    ## [0-9|-] : get one or more (+) numbers or dashes that are...
                                    ## (?<=PH) : preceeded by "PH"
  lapply(function(x) paste(unique(x), collapse = ", ")) %>%  ## collapse extracted digits into one string; unique(x) to make sure links are not repeated
  unlist()
cases_doh$link_desc <- case_when(cases_doh$link_desc %in% c("NA", "") ~ NA_character_,
                                 str_detect(cases_doh$link_desc, "known COVID-19 case" ) ~ "Undisclosed contact", ## Some cases have exposure to COVID-19 case but did not specify who
                                 TRUE ~ cases_doh$link_desc)