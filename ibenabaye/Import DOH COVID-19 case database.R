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
# countries <- paste(world.cities$country.etc, collapse = "|")
# sapply(cases_doh$travel_history, 
#        function(x) {x %>%
#                     str_replace_all("[()]|[;]", "") %>%
#                     str_detect(countries)})
