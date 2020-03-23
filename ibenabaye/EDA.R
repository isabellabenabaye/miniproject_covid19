library(extrafont)
loadfonts(device = "win", quiet = TRUE)
fonttable <- fonttable()
cases_reddit

# Theme 
theme_set(theme_minimal())
theme <- theme_update(text = element_text(family = "Source Sans Pro", size = 13),
                      plot.title = element_text("Franklin Gothic", size = 25, color = "gray20"),
                      title = element_text("Source Sans Pro Light", size = 18, color = "gray20"),
                      
                      legend.text = element_text("Source Sans Pro Light", size = 12),
                      axis.line.x = element_line(color = "gray80"),
                      axis.line.y = element_line(color = "gray80")
                      )


## Distribution of cases
# by age & sex
cases_reddit %>% 
  mutate(age_groups = cut(age, seq(10,90,by=10)),
         status = fct_relevel(status, "Admitted", "Recovered", "Dead")) %>%   ## group ages
  ggplot(aes(y = age_groups, fill = status)) +
  labs(title = "Cases by age & health status") +
  xlab("") + ylab("Age") + labs(fill = "Health status") +
  geom_bar(position = position_stack(reverse = TRUE)) +  ## reorder fill colors
  scale_fill_manual(values = c("gold", "springgreen3", "firebrick1")) +
  scale_x_continuous(limits = c(0,65), expand = c(0,0)) +
  scale_y_discrete(labels = c("11 - 20", "21 - 30","31 - 40", "41 - 50", "51 - 60", "61 - 70", "71 - 80", "81 - 90"))
  
