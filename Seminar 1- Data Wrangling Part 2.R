
participants_data <- read.csv("participants_data.csv")

library(readr)
urlfile = "https://raw.githubusercontent.com/CWWhitney/teaching_R/master/participants_data.csv"
participants_data <- read_csv(url(urlfile))
participants_data
head(participants_data, 
     n = 4)
names(participants_data)
str(participants_data)
participants_data$age
library(dplyr)
library(tidyr)
library(magrittr)

install.packages("magrittr")

library(magrittr)
select(participants_data, academic_parents, working_hours_per_day)

library(dplyr)

filter(participants_data, working_hours_per_day >10)
filter(participants_data, working_hours_per_day >10 & letters_in_first_name >6)
rename(participants_data, name_length = letters_in_first_name)
mutate(participants_data, labor_mean = working_hours_per_day * mean(working_hours_per_day))

mutate(participants_data, commute = ifelse(km_home_to_office > 10, "commuter", "local"))

summarize(participants_data, mean(years_of_study), median(letters_in_first_name))

library(magrittr)
participants_data %>% group_by(research_continent) %>% summarize(mean(days_to_email_response), 
            median(letters_in_first_name), 
            max(years_of_study))

participants_data %>% mutate(response_speed = ifelse(days_to_email_response > 1, "slow", "fast")) %>% 
  group_by(response_speed) %>% 
  summarize(mean(number_of_siblings), 
            median(years_of_study), 
            max(letters_in_first_name))


library(purrr)

participants_data %>% split(.$gender) %>% 
  map(~ 
        lm(number_of_publications ~ 
             number_of_siblings, 
           data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")











