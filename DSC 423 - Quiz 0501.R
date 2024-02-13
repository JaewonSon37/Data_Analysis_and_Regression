clinton <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Week 5\\Data File\\clinton19921.csv")
head(clinton)

# 1
set.seed(123)
partition <- sample(2, nrow(clinton), replace = TRUE, prob = c(0.80, 0.20))
train <- clinton[partition == 1 ,]
test <- clinton[partition == 2 ,]

# 2
model1 <- lm(Percent.voting.for.Clinton.in.1992 ~ Median.Age + Mean.Savings + PerCapita.Income + 
               Percent.in.Poverty + Percent.Veterans + Percent.Female + Population.Density + 
               Percent.in.Nursing.Homes + Crime.Index, data = clinton)
summary(model1)

# 3
model1_train <- lm(Percent.voting.for.Clinton.in.1992 ~ Median.Age + Mean.Savings + PerCapita.Income + 
                     Percent.in.Poverty + Percent.Veterans + Percent.Female + Population.Density + 
                     Percent.in.Nursing.Homes + Crime.Index, data = train)
summary(model1_train)

# 4
prediction <- predict(model1, test)
actual <- test$Percent.voting.for.Clinton.in.1992
cor(prediction, actual)

# 5, 6, 7, 8, 9
library(MASS)

clinton$MS2 <- (clinton$Mean.Savings/1000) * (clinton$Mean.Savings/1000)
clinton$PIP2 <- clinton$Percent.in.Poverty * clinton$Percent.in.Poverty
clinton$PV2 <- clinton$Percent.Veterans * clinton$Percent.Veterans
clinton$PF2 <- clinton$Percent.Female * clinton$Percent.Female
clinton$PD2 <- clinton$Population.Density * clinton$Population.Density

model2 <- lm(Percent.voting.for.Clinton.in.1992 ~ Median.Age + Mean.Savings + PerCapita.Income + 
               Percent.in.Poverty + Percent.Veterans + Percent.Female + Population.Density + 
               Percent.in.Nursing.Homes + Crime.Index + MS2 + PIP2 + PV2 + PF2 + PD2, data = clinton)

backward <- stepAIC(model2, direction = "backward")

# 10, 11, 12

model3 <- lm(Percent.voting.for.Clinton.in.1992 ~ Median.Age + Mean.Savings + PerCapita.Income + 
               Percent.in.Poverty + Percent.Veterans + Percent.Female + Population.Density + 
               Percent.in.Nursing.Homes + Crime.Index + MS2 + PIP2 + PV2 + PF2 + PD2, data = clinton)

model4 <- lm(Percent.voting.for.Clinton.in.1992 ~ 1, data = clinton)
summary(model4)

forward <- stepAIC(model4, direction = "forward",
                   scope = list(upper = model3, lower = model4))