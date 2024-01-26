# Q3.a
percentage_less_than_24 <- pnorm(24, 28, 4)
percentage_less_than_32 <- pnorm(32, 28, 4)

percentage_between_24_32 <- (percentage_less_than_32 - percentage_less_than_24) * 100
percentage_between_24_32

# Q3.b
percentage_less_than_36 <- pnorm(36, 28, 4)

percentage_older_than_36 <- (1 - percentage_less_than_36) * 100
percentage_older_than_36

# Q4
mean <- 150000
sd <- 35000
z_score <- qnorm(0.99)

top_montly_sale <- mean + z_score * sd
top_montly_sale

# Q5
mean1 <- 45
mean2 <- 42
sd <- 15.5
n <- 35
alpha <- 0.05

z_score <- (mean2 - mean1) / (sd / sqrt(n))
z_score

critical_z_score <- qnorm(alpha)
critical_z_score

# Q6.a
quasar <- read.table("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Week 2\\Data File\\QUASAR.txt", header = TRUE)

model1 <- lm(RFEWIDTH ~ REDSHIFT, data = quasar)
summary(model1)

model2 <- lm(RFEWIDTH ~ LINEFLUX, data = quasar)
summary(model2)

model3 <- lm(RFEWIDTH ~ LUMINOSITY, data = quasar)
summary(model3)

model4 <- lm(RFEWIDTH ~ AB1450, data = quasar)
summary(model4)

model5 <- lm(RFEWIDTH ~ ABSMAG, data = quasar)
summary(model5)