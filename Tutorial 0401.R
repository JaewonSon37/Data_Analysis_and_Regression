kc_house_data <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Week 4\\Data File\\kc_house_data.csv")
head(kc_house_data)

str(kc_house_data)

summary(kc_house_data)

d <- kc_house_data[, -c(1, 2)]
head(d)

str(d)

plot(kc_house_data$long, kc_house_data$lat)

d <- kc_house_data[, -c(1, 2, 18, 19)]
head(d)

str(d)

model <- lm(price ~ ., data = d)
summary(model)

d <- kc_house_data[, -c(1, 2, 17, 18, 19)]
head(d)

str(d)

model <- lm(price ~ ., data = d)
summary(model)

d <- kc_house_data[, -c(1, 2, 14, 17, 18, 19)]
head(d)

str(d)

model <- lm(price ~ ., data = d)
summary(model)

d <- kc_house_data[, -c(1, 2, 7, 14, 17, 18, 19)]
head(d)

str(d)

model <- lm(price ~ ., data = d)
summary(model)

d <- kc_house_data[, -c(1, 2, 7, 13, 14, 17, 18, 19)]
head(d)

str(d)

model <- lm(price ~ ., data = d)
summary(model)

cor(d)

dim(d)

dsample <- d[sample(1:nrow(d), 1000, replace = FALSE),]
summary(dsample)

head(dsample)

str(dsample)

dim(kc_house_data)

dim(d)

dim(dsample)

plot(dsample)

d <- kc_house_data[, -c(1, 2, 7, 13, 14, 17, 18, 19)]
d$sqft_livingSQ <- (d$sqft_living)^2
dim(d)

str(d)

model <- lm(price ~ ., data = d)
summary(model)

d$sqft_lotSQ <- (d$sqft_lot/1000)^2
dim(d)

str(d)

model <- lm(price ~ ., data = d)
summary(model)

d$wat_bed <- d$waterfront * d$bedrooms
dim(d)

str(d)

model <- lm(price ~ ., data = d)
summary(model)
