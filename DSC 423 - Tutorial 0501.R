for (i in 1:5) {
  result <- runif(1)
  print(result)
}

set.seed(37)
for (i in 1:5) {
  result <- runif(1)
  print(result)
}

set.seed(37)
for (i in 1:5) {
  result <- runif(1)
  print(result)
}

kc_house_data <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Week 5\\Data File\\kc_house_data1.csv")
d <- kc_house_data[, -c(1, 2)]
head(d)

str(d)

dim(d)

partition <- sample(2, nrow(d), replace = TRUE, prob = c(0.80, 0.20))
head(partition)

head(partition == 1)

head(partition == 2)

train <- d[partition == 1, ]
head(train)

str(train)

dim(train)

test <- d[partition == 2, ]
head(test)

str(test)

dim(test)

model <- lm(price ~ ., data = train)
summary(model)

prediction <- predict(model, test)
head(prediction)

actual = test$price
head(actual)

cor(prediction, actual)

plot(prediction, actual)

library(DAAG)
library(MASS)

out <- cv.lm(data = d, form.lm = formula(price ~ .), plotit = "Observed", m = 3)

d <- kc_house_data[, -c(1, 2)]
model_full <- lm(price ~ ., data = d)
summary(model_full)

step <- stepAIC(model_full, direction = "backward")
summary(step)

step$anova

d <- kc_house_data[, -c(1, 2)]
model_full <- lm(price ~ ., data = d)
summary(model_full)

model_empty <- lm(price ~ 1, data = d)
summary(model_empty)

step <- stepAIC(model_empty, direction = "forward", scope = list(upper = model_full, lower = model_empty))
summary(step)