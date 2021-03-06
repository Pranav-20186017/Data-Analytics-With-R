IBM = read.csv("IBMStock.csv")
GE = read.csv("GEStock.csv")
ProcterGamble = read.csv("ProcterGambleStock.csv")
CocaCola = read.csv("CocaColaStock.csv")
Boeing = read.csv("BoeingStock.csv")
IBM$Date = as.Date(IBM$Date, "%m / %d / %y")
GE$Date = as.Date(GE$Date, "%m / %d / %y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m / %d / %y")
CocaCola$Date = as.Date(CocaCola$Date, "%m / %d / %y")
Boeing$Date = as.Date(Boeing$Date, "%m / %d / %y")
nrow(IBM)
nrow(GE)
IBM$Date[which.min(IBM$Date)]
GE$Date[which.max(GE$Date)]
summary(IBM$StockPrice)
mean(IBM$StockPrice)
summary(GE$StockPrice)
GE$StockPrice[which.min(GE$StockPrice)]
summary(CocaCola$StockPrice)
CocaCola$StockPrice[which.max(CocaCola$StockPrice)]
summary(Boeing$StockPrice)
sd(ProcterGamble$StockPrice)
plot(CocaCola$Date, CocaCola$StockPrice)
plot(CocaCola$Date, CocaCola$StockPrice, type = "l")
plot(CocaCola$Date, CocaCola$StockPrice, type = "l")
lines(ProcterGamble$Date, ProcterGamble$StockPrice)
plot(CocaCola$Date, CocaCola$StockPrice, type = "l", col = "red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col = "blue")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col = "blue", lty=2)

which.max(tapply(CocaCola$StockPrice, months(CocaCola$Date), mean))
