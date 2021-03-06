summary(CPS)
str(CPS)
nrow(CPS)
sort(table(CPS$State))
which.min(sort(table(CPS$State)))
which.max(sort(table(CPS$State)))
sum(table(CPS$Citizenship)[c(1,2)])/sum(table(CPS$Citizenship))
C = subset(CPS, Citizenship == "Citizen, Native" | Citizenship == "Citizen, Naturalized")
nrow(C)/nrow(CPS)
summary(CPS)
(116639+7073)/nrow(CPS)
names(which(table(CPS$Race,CPS$Hispanic)[,2]>=250))
names(which(apply(CPS,2,function(x)sum(is.na(x))>1)))
table(CPS$Region, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))
names(which(table(CPS$State, is.na(CPS$MetroAreaCode))[,1]==0))
length(names(which(table(CPS$State, is.na(CPS$MetroAreaCode))[,1]==0)))
names(which(table(CPS$State, is.na(CPS$MetroAreaCode))[,2]==0))
length(names(which(table(CPS$State, is.na(CPS$MetroAreaCode))[,2]==0)))
names(which.max(table(CPS$Region,is.na(CPS$MetroAreaCode))[,2]))
names(which.min(abs(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean)-0.3)))
names(which.max(sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean),decreasing = T)[-c(1,2)]))
nrow(MetroAreaCodes)
nrow(CountryCodes)
CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
na1=sum(is.na(CPS))
CPS = merge(CPS, MetroAreaCodes, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
colnames(CPS)[ncol(CPS)]
na2=sum(is.na(CPS))
abs(na1-na2)
names(table(CPS$MetroArea)[34])
names(which.max(tapply(CPS$Hispanic,CPS$MetroArea,mean)))
names(which(tapply(CPS$Race == "Asian", CPS$MetroArea, mean)>0.2))
length(names(which(t(tapply(CPS$Race == "Asian", CPS$MetroArea, mean)>0.2))))
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean))
names(which.min(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=TRUE)))
CPS = merge(CPS, CountryCodes, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
colnames(CPS)[ncol(CPS)]
na3=sum(is.na(CPS))
abs(na3-na1)
head(sort(table(CPS$Country),decreasing = T),5)
names(which.max(table(CPS$Country)==839))
table(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country != "United States")
table(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country != "United States")[2,2]/sum(table(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country != "United States")[2,])
names(which.max(tapply(CPS$Country == "India", CPS$MetroArea, sum, na.rm=TRUE)))
names(which.max(tapply(CPS$Country == "Brazil", CPS$MetroArea, sum, na.rm=TRUE)))
names(which.max(tapply(CPS$Country == "Somalia", CPS$MetroArea, sum, na.rm=TRUE)))