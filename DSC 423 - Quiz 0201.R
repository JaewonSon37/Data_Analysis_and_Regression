bears1985 <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Week 2\\Data File\\bears1985.csv")

bears1985$Ht <- bears1985$HtFt * 12 + bears1985$HtIn 

plot(bears1985$Ht, bears1985$Weight, main = '1985 Bears', xlab = 'Height', ylab = 'Weight')

model <- lm(bears1985$Weight ~ bears1985$Ht)
summary(model)

model <- lm(Weight ~ Ht, data = bears1985)
summary(model)

bears1985.pred <- predict(model, bears1985)
bears1985.pred
bears1985.pred[1]

model$residuals

bears1985.pred <- predict(model, bears1985, interval = "confidence")
bears1985.pred