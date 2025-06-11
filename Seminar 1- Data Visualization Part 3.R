
#Vizualization ####
participants_data <- read.csv("participants_data.csv")
participants_barplot <- table(participants_data$academic_parents)
barplot(participants_barplot)
library(ggplot2)
ggplot(data = participants_data, aes(x = age, y = number_of_siblings)) + geom_point()
?ggplot()
ggplot(data = participants_data, aes(x = letters_in_first_name, y = days_to_email_response)) + geom_point()
ggplot(data = participants_data, aes(x = letters_in_first_name, y = days_to_email_response, color = academic_parents, 
                                     size = working_hours_per_day)) + geom_point()




#Iris Data ####


data("iris")

View(iris)
str(iris)
library(ggplot2)
ggplot(data = iris, 
       aes(x = Sepal.Length, 
           y = Petal.Length, 
           color = Species, 
           size = Petal.Width))+ 
  geom_point()



#diamonds ####


library(ggplot2)

plot1 <- ggplot(data = diamonds, 
                aes(x = cut, y = clarity, 
                    alpha = 0.2)) +
  geom_point()
ggplot(data = diamonds,
       aes(x = log(depth),
           y = log(table),
           alpha = 0.2)) +
  geom_point()

library(dplyr)

dsmall <- top_n(diamonds, n = 100)

ggplot(data = dsmall, aes(x = carat, 
                          y = price, 
                          color = color)) + 
  geom_point()

ggplot(data = diamonds, aes(x = carat, 
                          y = price, 
                          color = color)) + 
  geom_point()

# diamonds 50####

dsmall <- top_n(diamonds, n = 50)

ggplot(data = dsmall, aes(x= carat, y = price)) + geom_point() + geom_smooth()
ggplot(data = diamonds, aes(x= carat, y = price)) + geom_point() + geom_smooth()

# geom_smooth options ####
dsmall <- top_n(diamonds, n = 50)

ggplot(data = dsmall, aes(x = carat, y = price)) + geom_point() + 
  geom_smooth(method = "glm")

#boxplot diamonds####
ggplot(data = diamonds, aes(x = cut, y = price/carat)) + geom_boxplot()


ggplot(data = diamonds, 
       aes(x = color, 
           y = price/carat)) + 
  geom_boxplot()+ 
  geom_jitter()


ggplot(data = diamonds, 
       aes(x = color, 
           y = price/carat, 
           alpha = I(0.1))) + 
  geom_boxplot()+ 
  geom_jitter()
ggplot(data = diamonds, 
       aes(x = carat)) +
  geom_density()

ggplot(data = diamonds, 
       aes(x = price, 
           color = cut, 
           alpha = I(0.5))) +
  geom_density()

ggplot(data = mpg, 
       aes(x = displ, 
           y = hwy,  
           color = class)) + 
  geom_point() +
  geom_smooth(method = "glm")

ggplot(mtcars, 
       aes(mpg, 
           y = hp, 
           col = gear)) +
  geom_point() +
  ggtitle("My Title") +
  labs(x = "the x label", 
       y = "the y label", 
       col = "legend title")


ggplot(data = mtcars) +
  aes(x = mpg) +
  labs(x = "the x label") +
  aes(y = hp) +
  labs(y = "the y label") +
  geom_point() +
  aes(col = gear) +
  labs(col = "legend title") +
  labs(title = "My Title")
part_data <- select_if(participants_data, 
                       is.numeric, na.rm = TRUE)


cormat <- round(cor(part_data), 
                digits = 1)

melted_cormat <- as.data.frame.table(cormat, 
                                     responseName = "value")


ggplot(data = melted_cormat, 
       aes(x = Var1,
           y = Var2,
           fill = value)) + 
  geom_tile()
na.omit(select_if(participants_data, 
                  is.numeric, na.rm = TRUE))


ggplot(data = melted_cormat, 
       aes(x = Var1,
           y = Var2,
           fill = value)) + 
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red")

ggplot(data = melted_cormat, 
       aes(x = Var1,
           y = Var2,
           fill = value)) + 
  geom_tile() +
  scale_fill_gradient2(midpoint = 0.5, low = "#075AFF",
                       mid = "#FFFFCC",
                       high = "#FF0000")


ggplot(data = melted_cormat, 
       aes(x = Var1,
           y = Var2,
           fill = value)) + 
  geom_tile() +
  scale_fill_gradient2(midpoint = 0.5, low = "#075AFF",
                       mid = "#FFFFCC",
                       high = "#FF0000") +
  theme_bw(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
 