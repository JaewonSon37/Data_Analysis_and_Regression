bodyfat <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Week 6\\Data File\\bodyfat.csv")
str(bodyfat)

dim(bodyfat)

summary(bodyfat)

plot(bodyfat)

cor(bodyfat)

model <- lm(BodyFat ~ Weight + Chest + Abdomen + 
              Hip + Thigh + Biceps, data = bodyfat)
summary(model)

model <- lm(BodyFat ~ Chest + Abdomen + Hip + Thigh + 
              Biceps, data = bodyfat)
summary(model)

model <- lm(BodyFat ~ Weight + Chest + Abdomen 
            + Thigh + Biceps, data = bodyfat)
summary(model)

model <- lm(BodyFat ~ Weight + Abdomen + Thigh + 
              Biceps, data = bodyfat)
summary(model)

model <- lm(BodyFat ~ Weight + Chest + Thigh + 
              Biceps, data = bodyfat)
summary(model)

model <- lm(BodyFat ~ Weight + Abdomen + Thigh, data = bodyfat)
summary(model)

model <- lm(BodyFat ~ Weight + Abdomen + Biceps, data = bodyfat)
summary(model)

model <- lm(BodyFat ~ Weight + Abdomen, data = bodyfat)
summary(model)

library(car)

model <- lm(BodyFat ~ Weight + Chest + Abdomen + Hip +
              Thigh + Biceps, data = bodyfat)
summary(model)
vif(model)

model <- lm(BodyFat ~ Chest + Abdomen + Hip +
              Thigh + Biceps, data = bodyfat)
summary(model)
vif(model)

library(readxl)

Wages <- read_excel("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Week 6\\Data File\\Wages.xlsx")
str(Wages)

dim(Wages)

summary(Wages)

plot(Wages)

cor(Wages)

Wages$RACE <- as.factor(Wages$RACE)
Wages$OCCUPATION <- as.factor(Wages$OCCUPATION)
Wages$SECTOR <- as.factor(Wages$SECTOR)
str(Wages)

model <- lm(WAGE ~ EDUCATION + SOUTH + SEX + 
              EXPERIENCE + UNION + AGE + RACE + 
              OCCUPATION + SECTOR + MARR, data = Wages)
summary(model)

hist(Wages$WAGE, breaks = 20)

hist(log(Wages$WAGE), breaks = 20)

model <- lm(log(WAGE) ~ EDUCATION + SOUTH + SEX +
              EXPERIENCE + UNION + AGE + RACE + 
              OCCUPATION + SECTOR + MARR, data = Wages)
summary(model)

model <- lm(log(WAGE) ~ EDUCATION + SOUTH + SEX +
              log(EXPERIENCE + 1) + UNION + AGE + RACE + 
              OCCUPATION + SECTOR + MARR, data = Wages)
summary(model)

model <- lm(log(WAGE) ~ EDUCATION + SOUTH + SEX + 
              log(EXPERIENCE + 1) + UNION + log(AGE) + RACE + 
              OCCUPATION + SECTOR + MARR, data = Wages)
summary(model)

model <- lm(log(WAGE) ~ log(EDUCATION) + SOUTH + SEX + 
              log(EXPERIENCE + 1) + UNION + AGE + RACE + 
              OCCUPATION + SECTOR + MARR, data = Wages)
summary(model)

model <- lm(log(WAGE) ~ log(EDUCATION) + SOUTH + SEX + 
              log(EXPERIENCE + 1) + UNION + AGE + OCCUPATION + 
              SECTOR + MARR, data = Wages)
summary(model)

model <- lm(log(WAGE) ~ log(EDUCATION) + SOUTH + SEX + 
              log(EXPERIENCE + 1) + UNION + AGE + OCCUPATION + 
              SECTOR, data = Wages)
summary(model)

model <- lm(log(WAGE) ~ log(EDUCATION) + SOUTH + SEX + 
              log(EXPERIENCE + 1) + UNION + OCCUPATION + SECTOR, data = Wages)
summary(model)