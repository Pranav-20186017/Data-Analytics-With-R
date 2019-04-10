pisaTrain = read.csv('pisa2009train.csv')
pisaTest = read.csv('pisa2009test.csv')
nrow(pisaTrain)
tapply(pisaTrain$readingScore, pisaTrain$male, mean,na.rm = TRUE)
summary(pisaTrain)
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)
nrow(pisaTrain)
str(pisaTrain)
lmScore= lm(readingScore ~ . , data = pisaTrain)
summary(lmScore)
SSE = sum(lmScore$residuals^2)
RMSE = sqrt(SSE/nrow(pisaTrain))
RMSE
redTest = predict(lmScore, newdata = pisaTest)
summary(redTest)
SSE = sum((redTest - pisaTest$readingScore)^2)
SSE
RMSE = sqrt(SSE/nrow(pisaTest))
RMSE
SST = sum((mean(pisaTrain$readingScore) - pisaTest$readingScore)^2)
SST
R2 = 1 - SSE/SST
R2
