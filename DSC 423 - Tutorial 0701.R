kc_house_data <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Week 7\\Data File\\kc_house_data2.csv", header = TRUE, stringsAsFactors = TRUE)
head(kc_house_data)

str(kc_house_data)

dim(kc_house_data)

summary(kc_house_data)

model <- lm(price ~ bedrooms + bathrooms + sqft_living +
              view + yr_built, data = kc_house_data)
summary(model)

residuals = model$residuals
head(residuals)

sum = sum(model$residuals)
sum

hist(model$residuals, breaks = 100)

mean = mean(model$residuals)
mean

sd = sd(model$residuals)
sd

resid_zscore = (model$residuals - mean) / sd
head(resid_zscore)

hist(resid_zscore, breaks = 100)

library(car)

durbinWatsonTest(model)

model <- lm(price ~ bedrooms + bathrooms + sqft_living +
              view + yr_built, data = kc_house_data)
summary(model)

plot(kc_house_data$bedrooms, model$residuals)

plot(kc_house_data$bedrooms, resid_zscore)

plot(kc_house_data$bathrooms, resid_zscore)

plot(kc_house_data$sqft_living, resid_zscore)

plot(kc_house_data$view, resid_zscore)

plot(kc_house_data$yr_built, resid_zscore)

plot(model)

model2 <- lm(log(price) ~ bedrooms + bathrooms + sqft_living +
               view + yr_built, data = kc_house_data)
summary(model2)

plot(model2)