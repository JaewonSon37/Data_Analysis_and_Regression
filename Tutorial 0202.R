cars <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Week 2\\Data File\\cars.csv")

head(cars)

plot(cars[, 2:7])

model1 <- lm(mpg ~ displacement, data = cars)
summary(model1)

model2 <- lm(mpg ~ horsepower, data = cars)
summary(model2)

model3 <- lm(mpg ~ weight, data = cars)
summary(model3)

model4 <- lm(mpg ~ acceleration, data = cars)
summary(model4)

model5 <- lm(mpg ~ cylinders, data = cars)
summary(model5)
