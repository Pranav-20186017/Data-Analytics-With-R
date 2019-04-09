summary(md1)
cor(climate_train)
summary(md2)
summary(md3)
R2
climate = read.csv("climate_change.csv")
train = subset(climate, Year <= 2006)
test = subset(climate, Year > 2006)
climatelm = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data=train)
summary(climatelm)
z = cor(train)
kable(z)
cor(train)
LinReg = lm(Temp ~ MEI + N2O + TSI + Aerosols, data=train)
summary(LinReg)
StepModel = step(climatelm)
summary(StepModel)
tempPredict = predict(StepModel, newdata = test)
SSE = sum((tempPredict - test$Temp)^2)
SST = sum( (mean(train$Temp) - test$Temp)^2)
R2 = 1 - SSE/SST
R2
library(magrittr)
summary(m1)$coef %>% 
  {rownames(.)[.[,4] < 0.05]} %>% `[`(-1) %>% 
  paste0(collapse=", ")