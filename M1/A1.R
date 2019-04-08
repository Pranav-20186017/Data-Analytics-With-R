data1=data[which(data$Year>2001),]
hist(data1$Year, breaks=100,main="crime rate",xlab="year")
data2=data[which(data$Year>=2005 & data$Year<=2008),]
hist(data2$Year, breaks=100,main="crime rate",xlab="year")
data3=data[which(data$Year>=2009 & data$Year<=2011),]
hist(data3$Year, breaks=100,main="crime rate",xlab="year")
boxplot(data$Year~data$Arrest,main="crime trend",xlab="Arrest",ylab="year")
table(data$Arrest, data$Year)[2,1]/sum(table(data$Arrest, data$Year)[,1])
table(data$Arrest, data$Year)[2,7]/sum(table(data$Arrest, data$Year)[,7])
table(data$Arrest, data$Year)[2,12]/sum(table(data$Arrest, data$Year)[,12])
sort(table(mvt$LocationDescription))
names(sort(table(data$LocationDescription),decreasing = T))[-3][1:5]
name=names(sort(table(data$LocationDescription),decreasing = T))[-3][1:5]
nrow(subset(data,LocationDescription %in% name))
Top5$LocationDescription = factor(Top5$LocationDescription)
Top5=subset(data,LocationDescription %in% name)
Top5$LocationDescription = factor(Top5$LocationDescription)
names(which.max(apply(table(Top5$LocationDescription,Top5$Arrest),1,function(x) x[2]/sum(x))))
names(which.max(table(Top5$LocationDescription,Top5$Weekday)[3,]))
names(which.min(table(Top5$LocationDescription,Top5$Weekday)[2,]))