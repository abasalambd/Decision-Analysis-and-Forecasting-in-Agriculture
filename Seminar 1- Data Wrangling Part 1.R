#Start Again
1+1

# Data Wrangling Part 1 ####
version






#Data Wrangling Part 2 ####

version
install.packages("tidyverse")

library(tidyverse)
browseVignettes("tidyverse")
browseVignettes(package = "dplyr")

# ?read.csv
read.csv("participants_data.csv")

participants_data <- read.csv("participants_data.csv")

head(participants_data)

names(participants_data)

str(participants_data)

participants_data$age

#dplyr ####
install.packages("dplyr")
library(dplyr)

#task
install.packages("ggplot2")
library(ggplot2)
?diamonds
head(diamonds)
str(diamonds)

library(tidyverse)

a <- select(diamonds, carat, price)
b <- filter(a, carat > 0.5)
c <- rename(b, cost = price)
mean_of_cost <- mean(c$cost)
d <- mutate(c, price_class = ifelse(cost > mean_of_cost, "expensive", "cheap"))

#it worked but did not understand the spliting as group?
e <- group_by(d, price_class)


#did not finish the summary









