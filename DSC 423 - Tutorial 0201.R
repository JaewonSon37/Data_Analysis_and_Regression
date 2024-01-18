cars <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Week 2\\Data File\\cars.csv")

head(cars)

plot(cars$weight, cars$mpg)

plot(cars$horsepower, cars$mpg)

plot(cars)

cars[1, 1]

cars[, 2:7]

help(lm)

model <- lm(cars$mpg ~ cars$displacement)
model

summary(model)

model <- lm(mpg ~ displacement, data = cars)
model

summary(model)

coefficients(model)
coef(model)

fitted(model)

residuals(model)
resid(model)