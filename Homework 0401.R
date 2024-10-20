# Q2.a
banking <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Week 4\\Data File\\BANKING.csv")
head(banking)

# Q2.b
plot(banking$Age, banking$Balance)
plot(banking$Education, banking$Balance)
plot(banking$Income, banking$Balance)
plot(banking$HomeVal, banking$Balance)
plot(banking$Wealth, banking$Balance)

# Q2.c
cor(banking)

# Q2.d
regression_banking <- lm(Balance ~ Age + Education + Income + HomeVal + Wealth, data = banking)
summary(regression_banking)

# Q2.f
update_regression_banking <- lm(Balance ~ Age + Education + Income + Wealth, data = banking)
summary(update_regression_banking)

# Q3.a
wateroil <- read.table("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Week 4\\Data File\\WATEROIL.txt", header = FALSE)
colnames(wateroil) <- unlist(wateroil[1, ])
wateroil <- wateroil[-1, ]
wateroil <- wateroil[, !(names(wateroil) %in% c("Experiment"))]
head(wateroil)

wateroil$Voltage <- as.numeric(wateroil$Voltage)
wateroil$Volume <- as.numeric(wateroil$Volume)
wateroil$Salinity <- as.numeric(wateroil$Salinity)
wateroil$Temperature <- as.numeric(wateroil$Temperature)
wateroil$Delay <- as.numeric(wateroil$Delay)
wateroil$Surfactant <- as.numeric(wateroil$Surfactant)
wateroil$SpanTriton <- as.numeric(wateroil$SpanTriton)
wateroil$SolidPart <- as.numeric(wateroil$SolidPart)

regression_wateroil <- lm(Voltage ~ ., data = wateroil)
summary(regression_wateroil)

regression_wateroil2 <- lm(Voltage ~ .^2, data = wateroil)
summary(regression_wateroil2)

regression_wateroil3 <- lm(Voltage ~ .^2 +
                             I(Volume^2) +
                             I(Salinity^2) +
                             I(Temperature^2) +
                             I(Delay^2) +
                             I(Surfactant^2) +
                             I(SpanTriton^2) +
                             I(SolidPart^2)
                             , data = wateroil)
summary(regression_wateroil3)

regression_wateroil3_selection <- step(regression_wateroil3, direction = "both")
summary(regression_wateroil3_selection)
