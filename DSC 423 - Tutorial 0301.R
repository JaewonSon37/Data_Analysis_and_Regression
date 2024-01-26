housing <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Week 3\\Data File\\housing.csv")
str(housing)

housing$ocean_proximity <- as.factor(housing$ocean_proximity)
str(housing)

summary(housing)

hist(housing$median_house_value)

hist(housing$median_income)

hist(housing$housing_median_age, breaks = 20)

cor(housing[, 3:9])

cor(housing[,3:9], use = "complete.obs")

plot(housing$housing_median_age, housing$median_house_value)

plot(housing$total_rooms, housing$median_house_value)

plot(housing$total_bedrooms, housing$median_house_value)

plot(housing$population, housing$median_house_value)

plot(housing$households, housing$median_house_value)

plot(housing$median_income, housing$median_house_value)

m1 <- lm(median_house_value ~ housing_median_age, data = housing)
summary(m1)

m2 <- lm(median_house_value ~ total_rooms, data = housing)
summary(m2)

m3 <- lm(median_house_value ~ total_bedrooms, data = housing)
summary(m3)

m4 <- lm(median_house_value ~ population, data = housing)
summary(m4)

m5 <- lm(median_house_value ~ households, data = housing)
summary(m5)

m6 <- lm(median_house_value ~ total_bedrooms, data = housing)
summary(m6)

m7 <- lm(median_house_value ~ housing_median_age +
           population + households + median_income, data = housing)
summary(m7)

m8 <- lm(median_house_value ~ housing_median_age +
           population + households + median_income +
           population * median_income, data = housing)
summary(m8)

m9 <- lm(median_house_value ~ housing_median_age +
           population + households + median_income +
           poly(median_income, 2), data = housing)
summary(m9)

m10 <- lm(median_house_value ~ housing_median_age +
            population + households + median_income +
            ocean_proximity, data = housing)
summary(m10)

