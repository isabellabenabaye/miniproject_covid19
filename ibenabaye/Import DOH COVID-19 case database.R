library(tidyverse)
library(jsonlite)

# Prepare Data
## import data from the official PH DOH tracker
json_file <- "https://services5.arcgis.com/mnYJ21GiFTR97WFg/arcgis/rest/services/PH_masterlist/FeatureServer/0/query?f=json&where=1%3D1&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=102100&cacheHint=true" ## url to the DOH tracker JSON file
doh_raw <- fromJSON(json_file)
cases_doh <- as_tibble(doh_raw$features$attributes) ## make the table
names(cases_doh) <- c("case", "caseID", "age", "sex", "nationality", "residence", "travel_history", "symptoms", "positive_date", "facility", "latitude", "longitude", "status", "link_desc", "updated","x","y") ## properly label the columns
cases_doh
