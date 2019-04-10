FluTrain = read.csv("FluTrain.csv")
str(FluTrain)
FluTrain = read.csv("FluTrain.csv")
FluTrain$Week[which.max(FluTrain$ILI)]
FluTrain$Week[which.max(FluTrain$Queries)]
hist(FluTrain$ILI)
plot(FluTrain$Queries ,log(FluTrain$ILI))
FluTrend1 = lm(log(ILI) ~ Queries, data=FluTrain)
summary(FluTrend1)
COR = cor(log(FluTrain$ILI), FluTrain$Queries)
COR^2
FluTest = read.csv("FluTest.csv")
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
PredTest1[11]
RE1 = (FluTest$ILI[11] - PredTest1[11])/FluTest$ILI[11]
SSE1 = sum((FluTest$ILI - PredTest1)^2)
RMSE1 = sqrt(SSE1/nrow(FluTest))
library(zoo)
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
summary(FluTrain)
plot(log(FluTrain$ILI), log(FluTrain$ILILag2))
FluTrend2 = lm(log(ILI) ~ log(ILILag2) + Queries, data=FluTrain)
summary(FluTrend2)
FluTest$ILILag2 = coredata(lag(zoo(FluTest$ILI), -2, na.pad=TRUE))
FluTest$ILILag2[1] = FluTrain$ILI[nrow(FluTrain)-1]
FluTest$ILILag2[2] = FluTrain$ILI[nrow(FluTrain)]
FluTest$ILILag2[1]
FluTest$ILILag2[2]
PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
SSE2 = sum((FluTest$ILI - PredTest2)^2)
RMSE2 = sqrt(SSE2/nrow(FluTest))
RMSE2