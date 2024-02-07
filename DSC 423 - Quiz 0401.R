clinton <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Analysis and Regression\\DSC 423 - Week 4\\Data File\\clinton1992.csv")
head(clinton)

# 2, 3
cor(clinton[, -1])

# 4, 5, 7, 8
model1 <- lm(Percent.voting.for.Clinton.in.1992 ~ Median.Age + Mean.Savings +
               PerCapita.Income + Percent.in.Poverty + Percent.Veterans + Percent.Female +
               Population.Density + Percent.in.Nursing.Homes + Crime.Index, data = clinton)
summary(model1)

# 9, 10, 12
model2 <- lm(Percent.voting.for.Clinton.in.1992 ~ Mean.Savings + Percent.in.Poverty +
               Percent.Veterans + Percent.Female + Population.Density, data = clinton)
summary(model2)

clinton$MS2 <- (clinton$Mean.Savings/1000) * (clinton$Mean.Savings/1000)
clinton$PIP2 <- clinton$Percent.in.Poverty * clinton$Percent.in.Poverty
clinton$PV2 <- clinton$Percent.Veterans * clinton$Percent.Veterans
clinton$PF2 <- clinton$Percent.Female * clinton$Percent.Female
clinton$PD2 <- clinton$Population.Density * clinton$Population.Density

model3 <- lm(Percent.voting.for.Clinton.in.1992 ~ Mean.Savings + Percent.in.Poverty +
               Percent.Veterans + Percent.Female + Population.Density +
               MS2 + PIP2 + PV2 + PF2 + PD2, data = clinton)
summary(model3)


