cars <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Week 1\\Data File\\cars.csv")

head(cars)

head(cars$model)

head(cars$mpg)

mean(cars$mpg, na.rm = TRUE)

mean(cars$horsepower, na.rm = TRUE)

summary(cars)

boxplot(cars$horsepower, main = "Horsepower")

boxplot(cars$weight, main = "Weight")

hist(cars$horsepower)

hist(cars$mpg, breaks = 20, main = "Cars", xlab = "MPG", freq = FALSE)
mean <- mean(cars$mpg, na.rm = TRUE)
mean
sd <- sd(cars$mpg, na.rm = TRUE)
sd
curve(dnorm(x, mean, sd), add = TRUE, col = 'darkblue', lwd = 2)
