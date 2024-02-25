# 1
seatpos <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Week 7\\Data File\\seatpos.csv")

# 2, 3
plot(seatpos)

# 4, 5
cor(seatpos)

# 6
model.seatpos <- lm(hipcenter ~ ., data = seatpos)
summary(model.seatpos)

# 7
library(car)

vif(model.seatpos)

# 9
flights <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Week 7\\Data File\\flights.csv")

# 10
flights <- flights[complete.cases(flights), ]

# 11
flights$delay <- flights$dep_time - flights$sched_dep_time

# 12
flights$late <- flights$arr_time - flights$sched_arr_time

# 13
flights$time <- flights$arr_time - flights$dep_time
flights$speed <- flights$distance / flights$time

# 14
flightsJan <- flights[flights$month == 1, ]
summary(flightsJan)

# 15
flightsJun <- flights[flights$month == 6, ]
summary(flightsJun)

# 16
flightsUA <- flights[flights$carrier == 'UA', ]
summary(flightsUA)

# 17, 18, 19
wafer <- read.table("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Week 7\\Data File\\WAFER.txt", header = TRUE)

model.wafer <- lm(FAILTIME ~ TEMP, data = wafer)
summary(model.wafer)

# 20
model.wafer$residuals

plot( wafer$TEMP,model.wafer$residuals )

# 21, 22, 23, 24
wafer$TEMP_SQ <- wafer$TEMP * wafer$TEMP

model.wafer2 <- lm(FAILTIME ~ TEMP + TEMP_SQ, data = wafer)
summary(model.wafer2)

# 26, 27, 28
appliance <- read.table("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Week 7\\Data File\\APPLIANCE.txt", header = TRUE)

model.appliance <- lm(BUYPROP ~ AGE, data = appliance)
summary(model.appliance)

# 29
plot(model.appliance$fitted.values, model.appliance$residuals)

# 30, 31
appliance$Y_TRANS <- asin(sqrt(appliance$BUYPROP))

model.appliance2 <- lm(Y_TRANS ~ AGE, data = appliance)
summary(model.appliance2)

# 32
gasturbine <- read.table("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Week 7\\Data File\\GASTURBINE.txt", header = TRUE)

model.gasturbine <- lm(HEATRATE ~ RPM + CPRATIO, data = gasturbine)
summary(model.gasturbine)

# 33, 34
hist(model.gasturbine$residuals)

# 35
set.seed(37)
x <- rnorm(100)
qqnorm(x)
qqline(x)

# 36
qqnorm(model.gasturbine$residuals)
qqline(model.gasturbine$residuals)

# 37, 38, 39
misswork <- read.table("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Week 7\\Data File\\MISSWORK.txt", header = TRUE)

model.misswork <- lm(HOURS ~ WAGES, data = misswork)
summary(model.misswork)

# 40
plot(misswork$WAGES, model.misswork$residuals)

# 41
misswork.clean <- misswork[-13, ]

model.misswork2 <- lm(HOURS ~ WAGES, data = misswork.clean)
summary(model.misswork2)