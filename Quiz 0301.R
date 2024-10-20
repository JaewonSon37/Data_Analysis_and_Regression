monet <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Week 3\\Data File\\Monet.csv")

# 1, 2, 3
model <- lm(PRICE ~ HEIGHT + WIDTH, data = monet)
summary(model)

# 8, 9
monet$H_W <- monet$HEIGHT * monet$WIDTH
model <- lm(PRICE ~ HEIGHT + WIDTH + H_W, data = monet)
summary(model)

# 11
monet$Hsq <- monet$HEIGHT * monet$HEIGHT
monet$Wsq <- monet$WIDTH * monet$WIDTH
model <- lm(PRICE ~ HEIGHT + WIDTH + Hsq + Wsq, data = monet)
summary(model)

# 13, 14
model <- lm(PRICE ~ HEIGHT + WIDTH + SIGNED, data = monet)
summary(model)

# 17
monet$H1 <- (monet$HOUSE == 1) * 1
monet$H2 <- (monet$HOUSE == 2) * 1
model <- lm(PRICE ~ HEIGHT + WIDTH + H1 + H2, data = monet)
summary(model)
