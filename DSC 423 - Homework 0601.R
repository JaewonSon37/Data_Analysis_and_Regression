# Q1
pisa <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Week 6\\Data File\\Pisa2009.csv")
head(pisa)

# Q1.a
pisa <- subset(pisa, select = - X)
head(pisa)

str(pisa)

vars_to_factor <- c("grade", "male", "raceeth", "preschool", "expectBachelors",
                    "motherHS", "motherBachelors", "motherWork",
                    "fatherHS", "fatherBachelors", "fatherWork",
                    "selfBornUS", "motherBornUS", "fatherBornUS",
                    "englishAtHome", "computerForSchoolwork", "read30MinsADay",
                    "schoolHasLibrary", "publicSchool", "urban")
pisa[vars_to_factor] <- lapply(pisa[vars_to_factor], as.factor)

# Q1.b
dummy_variables <- model.matrix( ~ . +0, data = pisa[vars_to_factor])
pisa <- cbind(pisa, dummy_variables)
pisa <- pisa[, !(names(pisa) %in% vars_to_factor)]
pisa <- subset(pisa, select = - grade8)

# Q1.c
library(car)

multicolinearity <- lm(readingScore ~ ., data = pisa)
vif(multicolinearity)

vif_over_10 <- c("grade9", "grade10", "grade11",
                 "raceethBlack", "raceethHispanic", "raceethWhite")
pisa <- pisa[, !(names(pisa) %in% vif_over_10)]

multicolinearity <- lm(readingScore ~ ., data = pisa)
vif(multicolinearity)

# Q1.d
library(MASS)

model1 <- lm(readingScore ~ ., data = pisa)
stepwise_model <- step(model1, direction = "both", trace = FALSE)
summary(stepwise_model)

pisa <- subset(pisa, select = c(readingScore, minutesPerWeekEnglish, schoolSize, male1,
                                raceethAsian, expectBachelors1, motherBachelors1, fatherHS1, 
                                fatherBachelors1, fatherWork1, fatherBornUS1, englishAtHome1,
                                computerForSchoolwork1, read30MinsADay1, publicSchool1, urban1))

model2 <- lm(readingScore ~ .^2, data = pisa)
summary(model2)

model3 <- lm(readingScore ~ .^2 + I(minutesPerWeekEnglish^2) + I(schoolSize^2) + I(male1^2) +
               I(raceethAsian^2) + I(expectBachelors1^2) + I(motherBachelors1^2) + I(fatherHS1^2) + 
               I(fatherBachelors1^2) + I(fatherWork1^2) + I(fatherBornUS1^2) + I(englishAtHome1^2) + 
               I(computerForSchoolwork1^2) + I(read30MinsADay1^2) + I(publicSchool1^2) + I(urban1^2), data = pisa)
summary(model3)

# Q1.e
stepwise_model <- step(model3, direction = "both", trace = FALSE)
summary(stepwise_model)

model4 <- lm(readingScore ~ minutesPerWeekEnglish + schoolSize + male1 + expectBachelors1 + 
               motherBachelors1 + fatherHS1 + fatherBornUS1 + read30MinsADay1 + 
               publicSchool1 + I(minutesPerWeekEnglish^2) + minutesPerWeekEnglish:publicSchool1 + 
               schoolSize:fatherBornUS1 + schoolSize:publicSchool1 + male1:read30MinsADay1 + 
               expectBachelors1:motherBachelors1 + motherBachelors1:fatherHS1 + 
               motherBachelors1:read30MinsADay1 + motherBachelors1:publicSchool1 + 
               fatherBornUS1:publicSchool1, data = pisa)
summary(model4)

model5 <- lm(readingScore ~ minutesPerWeekEnglish + schoolSize + male1 + expectBachelors1 + 
               motherBachelors1 + fatherHS1 + fatherBornUS1 + read30MinsADay1 + publicSchool1 +
               I(minutesPerWeekEnglish^2) + minutesPerWeekEnglish:publicSchool1 + 
               schoolSize:fatherBornUS1 + schoolSize:publicSchool1 + expectBachelors1:motherBachelors1 + 
               motherBachelors1:fatherHS1 + fatherBornUS1:publicSchool1, data = pisa)
summary(model5)

# Q1.f
library(caret)

set.seed(37)
index <- createDataPartition(pisa$readingScore, p = 0.7, list = FALSE)
train_set <- pisa[index, ]
test_set <- pisa[-index, ]

# Q1.g
final_model <- lm(readingScore ~ minutesPerWeekEnglish + schoolSize + male1 + expectBachelors1 + 
                    motherBachelors1 + fatherHS1 + fatherBornUS1 + read30MinsADay1 + publicSchool1 +
                    I(minutesPerWeekEnglish^2) + minutesPerWeekEnglish:publicSchool1 + 
                    schoolSize:fatherBornUS1 + schoolSize:publicSchool1 + expectBachelors1:motherBachelors1 + 
                    motherBachelors1:fatherHS1 + fatherBornUS1:publicSchool1, data = train_set)
summary(final_model)

predictions <- predict(final_model, newdata = test_set)

rmse <- sqrt(mean((test_set$readingScore - predictions)^2))
print(paste("RMSE:", rmse))

mae <- mean(abs(test_set$readingScore - predictions))
print(paste("MAE:", mae))