library(tidyverse)
library(extrafont)
loadfonts(device = "win", quiet = TRUE)
fonttable <- fonttable()

# Sourcing codes
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('Import DOH COVID-19 case database.R')
source('Import reddit COVID-19 case database.R')

# Join tables, with cases_doh as the main table
cases_full <- 
  left_join(cases_doh %>% 
              select(case, 
                     caseID, 
                     age, 
                     sex, 
                     nationality, 
                     residence, 
                     facility, 
                     travel, 
                     link_desc, 
                     positive_date),
            cases_reddit %>%
              select(case,
                     status,
                     status_admitted,
                     onset_date,
                     others,
                     asymptomatic,
                     other_conditions),
            by = c("case" = "case"))

# NOTES: as of 22MAR2020
## for fields that don't match, DOH data will be used
## ages don't match (5 discrepancies)
## confirmed/positive dates don't match (5 discrepancies)
## sexes don't match (9 discrepancies)
## residences don't match (10 discrepancies)
## facilities don't match (7 discrepancies)

# NOTES: as of 5APR2020
## DOH was used for repeating columns
## For features not in DOH (e.g., status, status_admitted, etc.), reddit columns were appended.
## Plan was disccused found in Notion - Data Issues page.

# Theme 
theme_set(theme_minimal())
theme <- theme_update(text = element_text(family = "Source Sans Pro", size = 13),
                      plot.title = element_text("Franklin Gothic", size = 30, color = "gray20"),
                      title = element_text("Source Sans Pro Light", size = 18, color = "gray20"),
                      
                      legend.text = element_text("Source Sans Pro Light", size = 12),
                      axis.line.x = element_line(color = "gray80"),
                      axis.line.y = element_line(color = "gray80")
                      )



## Distribution of cases
# by age & sex
cases_full %>% 
  mutate(age_groups = cut(age, seq(10,90,by=10)),
         status = fct_relevel(status, "Admitted", "Recovered", "Dead")) %>%   ## group ages
  ggplot(aes(y = age_groups, fill = status)) +
  labs(title = "Cases by age & health status") +
  xlab("") + ylab("Age") + labs(fill = "Health status") +
  geom_bar(position = position_stack(reverse = TRUE)) +  ## reorder fill colors
  scale_fill_manual(values = c("gold", "springgreen3", "firebrick1")) +
  scale_x_continuous(expand = expansion(mult = c(0,0.05))) +
  scale_y_discrete(labels = c("11 - 20", "21 - 30","31 - 40", "41 - 50", "51 - 60", "61 - 70", "71 - 80", "81 - 90"))


# cumulative number of cases per day
# data
cases_by_date <- cases_full  %>% 
  group_by(positive_date) %>% 
  summarise(count = n()) %>% 
  mutate(total = cumsum(count))

# total by day
cases_by_date %>% 
  ggplot(aes(x = positive_date, y = total)) +
  labs(title = "Confirmed cases") +
  xlab("") + ylab("Total confirmed cases") +
  geom_col(fill = "orange") +
  scale_y_continuous(expand = expansion(mult = c(0,.05)))


