library(glmnet)

swiss <- datasets::swiss
head(swiss)

str(swiss)

dim(swiss)

summary(swiss)

x <- as.matrix(swiss[, 1:5])
x

y <- as.matrix(swiss[, 6])
y

set.seed(123)
ridge <- cv.glmnet(x, y, family = "gaussian", alpha = 0)
plot(ridge)

ridge$lambda.min

coef(ridge, s = ridge$lambda.min)

set.seed(123)
lasso <- cv.glmnet(x, y, family = "gaussian", alpha = 1)
plot(lasso)

lasso$lambda.min

coef(ridge, s = lasso$lambda.min)

set.seed(123)
elasticnet <- cv.glmnet(x, y, family = "gaussian", alpha = 0.5)
plot(elasticnet)

elasticnet$lambda.min

coef(ridge, s = elasticnet$lambda.min)

admit <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Week 8\\Data File\\admit.csv")
head(admit)

str(admit)

dim(admit)

summary(admit)

admit$rank <- factor(admit$rank)
admit$admit <- factor(admit$admit)
str(admit)

dim(admit)

model <- glm(admit ~ gre + gpa + rank, data = admit, family = "binomial")
summary(model)