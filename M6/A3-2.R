baseball = read.csv("baseball.csv")
nrow(baseball) 
table(baseball$Year) 
length(table(baseball$Year))
nrow(subset(baseball, Playoffs == 1))
table(baseball$Year)
table(table(baseball$Year))
PlayoffTable = table(baseball$Year)
str(names(PlayoffTable))
PlayoffTable[c("1990", "2001")] correct
baseball$NumCompetitors = PlayoffTable[as.character(baseball$Year)]
table(baseball$NumCompetitors)
baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)
table(baseball$WorldSeries)
summary(glm(WorldSeries~Year, data=baseball, family="binomial"))
LogModel = glm(WorldSeries ~ Year + RA + RankSeason + NumCompetitors, data=baseball, family=binomial)
summary(LogModel)
cor(baseball$Year, baseball$RA)
cor(baseball[c(“Year”, “RA”, “RankSeason”, “NumCompetitors”)])
Model1 = glm(WorldSeries ~ Year + RA, data=baseball, family=binomial)

Model2 = glm(WorldSeries ~ Year + RankSeason, data=baseball, family=binomial)

Model3 = glm(WorldSeries ~ Year + NumCompetitors, data=baseball, family=binomial)

Model4 = glm(WorldSeries ~ RA + RankSeason, data=baseball, family=binomial)

Model5 = glm(WorldSeries ~ RA + NumCompetitors, data=baseball, family=binomial)

Model6 = glm(WorldSeries ~ RankSeason + NumCompetitors, data=baseball, family=binomial)

