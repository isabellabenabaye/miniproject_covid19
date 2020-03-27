## Import data
source("Import reddit COVID-19 case database.R")
source("Import DOH COVID-19 case database.R")


## Load libraries
library(extrafont)
library(ggrepel)

loadfonts(device = "win", quiet = TRUE)
# fonttable <- fonttable()


## Theme 
theme_set(theme_minimal())
theme <- theme_update(text = element_text(family = "Source Sans Pro", size = 13),
                      plot.title = element_text("Franklin Gothic", size = 30, color = "gray20"),
                      title = element_text("Source Sans Pro Light", size = 18, color = "gray20"),
                      plot.subtitle = element_text("Source Sans Pro Light", size = 10, face = "italic", color = "gray20"),
                      legend.text = element_text("Source Sans Pro Light", size = 12),
                      axis.line.x = element_line(color = "gray80"),
                      axis.line.y = element_line(color = "gray80")
                      )


## Join tables, with cases_doh as the main table
cases_full <- cases_doh %>% 
  left_join(cases_reddit, by = c("case" = "case"), suffix = c("","_r"))
# NOTES: as of 22MAR2020
## for fields that don't match, DOH data will be used
## ages don't match (5 discrepancies)
## confirmed/positive dates don't match (5 discrepancies)
## sexes don't match (9 discrepancies)
## residences don't match (10 discrepancies)
## facilities don't match (7 discrepancies)

# status counts
cases_full %>% 
  group_by(status_r) %>% 
  summarise(count = n())


## Make tables
# number of cases by age & sex
cases_age_status <- cases_full %>% 
  mutate(age_groups = cut(age, seq(10,90,by=10)),
         status_r = fct_relevel(status_r, "Admitted", "Recovered", "Dead"))   %>% 
  group_by(age_groups) %>% 
  summarise(count = n())

# number of cases per day & running total
cases_date <- cases_full  %>% 
  group_by(positive_date) %>% 
  summarise(count = n()) %>% 
  mutate(total = cumsum(count))

# number of cases per day & status
cases_date_status <- cases_full  %>% 
  mutate(status_r = fct_explicit_na(status_r)) %>% 
  group_by(positive_date, status_r) %>%
  count() %>% 
  spread(status_r,n) %>%
  mutate_all(function (x) ifelse(is.na(x), 0, x)) %>% 
  mutate(count = sum(`Admitted`, `Dead`, `Recovered`)) %>% 
  ungroup() %>% 
  mutate(total = cumsum(count),
         recovered = cumsum(Recovered),
         dead = cumsum(Dead)) %>% 
  select(positive_date, total, recovered, dead) %>% 
  gather("status", "count", total, recovered, dead)


## Distribution of cases
# by age & sex
cases_full %>% 
  mutate(age_groups = cut(age, seq(10,90,by=10)),
         status_r = fct_relevel(status_r, "Admitted", "Recovered", "Dead")) %>%   ## group ages
  ggplot(aes(y = age_groups, fill = status_r)) +
  labs(title = "Cases by age & health status") +
  xlab("") + ylab("Age") + labs(fill = "Health status") +
  geom_bar(position = position_stack(reverse = TRUE)) +  ## reorder fill colors
  scale_fill_manual(values = c("gold", "springgreen3", "firebrick1"), na.value = "gray90") +
  scale_x_continuous(expand = expansion(mult = c(0,0.02))) +
  scale_y_discrete(labels = c("11 - 20", "21 - 30","31 - 40", "41 - 50", "51 - 60", "61 - 70", "71 - 80", "81 - 90"))
 

# cumulative number of cases per day
cases_date %>% 
  ggplot(aes(x = positive_date, y = total)) +
  labs(title = "Confirmed cases") +
  xlab("") + ylab("Total confirmed cases") +
  geom_col(fill = "orange") +
  scale_x_date(expand = expansion(mult = c(0,0)), date_breaks = "5 days", date_labels = "%b %e", limits = c(min(cases_date$positive_date), max = max(cases_date$positive_date))) +
  scale_y_continuous(expand = expansion(mult = c(0,.05)))


# cumulative number of cases per day with labels
cases_date %>% 
  ggplot(aes(x = positive_date, y = total)) +
  labs(title = "Confirmed cases") +
  xlab("") + ylab("Total confirmed cases") +
  geom_col(fill = "orange") +
  scale_x_date(expand = expansion(mult = c(0.03,0)), date_breaks = "5 days", date_labels = "%b %e", limits = c(min(cases_date$positive_date), max = max(cases_date$positive_date))) +
  scale_y_continuous(expand = expansion(mult = c(0,.05))) +
  
  # Three Chinese national cases
  annotate(geom = "curve", # curve & arrow
           x = as.Date("2020-02-01"), y = 40, 
           xend = as.Date("2020-01-30"), yend = 5, 
           curvature = .23, color = "grey60", size = 0.4,  
           arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "label", x = as.Date("2020-02-01"), y = 40, size = 5, #label
           label = "Three Chinese \nnational cases", 
           family = "Source Sans Pro Light",
           hjust = "left",
           lineheight = 0.9,
           color = "grey50",
           label.size = NA) +
  
  # Community quarantine in Metro Manila
  annotate(geom = "curve", 
           x = as.Date("2020-03-11"), y = 175, 
           xend = as.Date("2020-03-15"), yend = 133, 
           curvature = -.23, color = "grey60", size = 0.4, 
           arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "label", x = as.Date("2020-03-11"), y = 175, size = 5,
           label = "Community quarantine\n in Metro Manila", 
           family = "Source Sans Pro Light",
           hjust = "right",
           lineheight = 0.9,
           color = "grey50",
           label.size = NA) +
  
  # Enhanced community quarantine in Luzon
  annotate(geom = "curve", 
           x = as.Date("2020-03-12"), y = 230, 
           xend = as.Date("2020-03-16"), yend = 187, 
           curvature = -.23, color = "grey60", size = 0.4, 
           arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "label", x = as.Date("2020-03-12"), y = 230, size = 5,
           label = "Enhanced community\n quarantine in Luzon", 
           family = "Source Sans Pro Light",
           hjust = "right",
           lineheight = 0.9,
           color = "grey50",
           label.size = NA) 

ggsave("Confirmed cases.png", device = "png")
  
  
# cumulative number of cases per day & status
cases_date_status %>% 
  ggplot(aes(x = positive_date, y = count)) +
  labs(title = "Confirmed cases") +
  xlab("") + ylab("Number of cases") + 
  geom_line(aes(color = status), size = 1.3) +
  geom_point(aes(color = status), size = 3.3) +
  scale_color_manual(breaks = c("total", "recovered", "dead"),
                     values = c("gold", "springgreen3", "firebrick1"),
                     labels = c("Total", "Recovered", "Dead")) +
  theme(legend.title = element_blank(),)


# new cases per day
cases_date %>% 
  ggplot(aes(x = positive_date, y = count)) +
  labs(title = "New confirmed cases per day") +
       #subtitle = "Philippines") +
  xlab("") + ylab("New confirmed cases")+ labs(fill = "") +
  geom_col(aes(fill = count)) +
  scale_y_continuous(expand = expansion(mult = c(0,.05))) +
  scale_x_date(expand = expansion(mult = c(0,0)), date_breaks = "5 days", date_labels = "%b %e", limits = c(min(cases_date$positive_date), max = max(cases_date$positive_date))) +
  scale_fill_gradient(low = "yellow", high = "red", na.value = NA)




  