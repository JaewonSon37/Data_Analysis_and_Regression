# 1
HeartDiseaseRate <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Week 8\\Data File\\HeartDiseaseRate.csv")

# 2
summary(HeartDiseaseRate)

# 3
x <- as.matrix(HeartDiseaseRate[, 2:30])
y <- as.double(HeartDiseaseRate[, 1])

# 6
library(glmnet)

set.seed(123)
lasso <- cv.glmnet(x, y, family = "gaussian", alpha = 1)
plot(lasso)

lasso$lambda.min

# 7, 8
coef(lasso, s = lasso$lambda.min)

# 9
coef(lasso, s = 2)

# 11
Titanic_abr <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Week 8\\Data File\\Titanic_abr.csv")

# 12
Titanic_abr$Survived <- as.factor(Titanic_abr$Survived)
Titanic_abr$Pclass <- as.factor(Titanic_abr$Pclass)
Titanic_abr$Name <- NULL

# 13, 14
summary(Titanic_abr)

# 15, 16, 17, 18
model <- glm(Survived ~ ., family = "binomial", data = Titanic_abr)
summary(model)

# 19, 20
exp(coef(model)) - 1
