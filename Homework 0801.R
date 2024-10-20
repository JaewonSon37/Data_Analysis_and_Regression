# Q4
ex7_20 <- read.table("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Week 8\\Data File\\EX7_20.txt", header = TRUE)

# Q4.a
plot(ex7_20$X, ex7_20$Y, main = "Scatter Plot Between X and Y")

cor(ex7_20$X, ex7_20$Y)

# Q4.b
ex7_20$log_X <- log(ex7_20$X)
ex7_20$log_Y <- log(ex7_20$Y)

plot(ex7_20$log_X, ex7_20$log_Y, main = "Scatter Plot Between Log-transformed X and Y")

cor(ex7_20$log_X, ex7_20$log_Y)

# Q4.c
model.ex7_20 <- lm(ex7_20$log_Y ~ ex7_20$log_X, data = ex7_20)
summary(model.ex7_20)

# Q6
boilers <- read.table("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Week 8\\Data File\\BOILERS.txt", header = TRUE)
boilers$Drum <- as.factor(boilers$Drum)

model.boilers <- lm(Boiler ~ ., data = boilers)
residual.boilers <- residuals(model.boilers)

qqnorm(residual.boilers)
qqline(residual.boilers)

hist(residual.boilers, main = "Histogram of Residuals")

# Q7
ex8_1 <- read.table("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Week 8\\Data File\\EX8_1.txt", header = TRUE)
ex8_2 <- read.table("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Week 8\\Data File\\EX8_2.txt", header = TRUE)

# Q7.a
model.ex8_1 <- lm(Y ~ ., data = ex8_1)
plot(model.ex8_1)

# Q7.b
model.ex8_2 <- lm(Y ~ ., data = ex8_2)
plot(model.ex8_2)

# Q9 
misswork1 <- read.table("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Week 8\\Data File\\MISSWORK1.txt", header = TRUE)

# Q9.a
model.misswork1 <- lm(HOURS ~ WAGES, data = misswork1)
summary(model.misswork1)

# Q9.b
plot(model.misswork1)

# Q9.c
misswork2 <- misswork1[-c(13), ]
model.misswork2 <- lm(HOURS ~ WAGES, data = misswork2)
summary(model.misswork2)

# 11
growth <- read.table("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Week 8\\Data File\\GROWTH.txt", header = TRUE)

# 11.a
plot(growth$HOURS, growth$CELLS, main = "Scatter Plot Between Hours and Cells")

# 11.c
model.growth <- lm(CELLS ~ HOURS + I((HOURS - 70) * (HOURS > 70)),
                   data = growth)
summary(model.growth)

# 11.d
anova(model.growth)

# 11.f
estimate_hours_below_70 <- 0.22214
std.error_hours_below_70 <- 0.03404
estimate_hours_above_70 <- -0.27290
std.error_hours_above_70 <- 0.06308

t_value_hours_below_70 <- estimate_hours_below_70 / std.error_hours_below_70
t_value_hours_above_70 <- estimate_hours_above_70 / std.error_hours_above_70

p_value_hours_below_70 <- 2 * pt(abs(t_value_hours_below_70), df = 14, lower.tail = FALSE)
p_value_hours_above_70 <- 2 * pt(abs(t_value_hours_above_70), df = 14, lower.tail = FALSE)

print(paste("t-value of less than 70 hours:", t_value_hours_below_70))
print(paste("p-value of less than 70 hours:", p_value_hours_below_70))
print(paste("t-value of more than 70 hours:", t_value_hours_above_70))
print(paste("p-value of more than 70 hours:", p_value_hours_above_70))
