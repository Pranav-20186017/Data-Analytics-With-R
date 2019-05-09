#Question-1:
bank = read.csv("bank-full.csv")
str(bank)
library(caTools)
set.seed(1000)
spl = sample.split(bank$y,SplitRatio=0.6)
train = subset(bank,spl==TRUE)
test = subset(bank,spl==FALSE)
mod1 = glm(y~age+balance+campaign+duration,data=train,family="binomial")
summary(mod1)
mod1 = glm(y~age+balance+duration,data=train,family="binomial")
summary(mod2)
pred1 = predict(mod1,newdata=test,type="response")
table(test$y,pred1>0.5)
library(rpart)
library(rpart.plot)
bankCART = rpart(y~age+balance+duration,data=bank,method="class",minbucket=2)
prp(bankCART)
data = subset(bank, marital=="married" & job=="technician")
nrow(data)/nrow(bank)
#0.08962421


#Question-2:
movies = read.table("Movies.txt", header=FALSE, sep="|",quote="\"")
colnames(movies) = c("ID","Title","ReleaseDate","VideoReleaseDate","IMDB","Unknown","Action","Adventure","Animation","Childrens","Comedy","Crime","Documentary","Drama","Fantasy","FilmNoir","Horror","Musical","Mystery","Romance","SciFi","Thriller","War","Western")
str(movies)
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL
movies = unique(movies)
nrow(subset(movies, Action==1 & Horror == 1))
#13
distances = dist(movies[2:20],method="euclidian")
clusterMovies = hclust(distances,method="ward.D")
plot(clusterMovies)
which.max(clusterGroups)
which.min(clusterGroups)








#Question-3:
energy = read.csv("energy_readings.csv",stringAsFactors=FALSE)
table(energy$responsive)
139/nrow(energy)

library(tm)
library(SnowballC)
corpus = Corpus(VectorSource(energy$email))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm, 0.95)
labeledTerms = as.data.frame(as.matrix(dtm))
labeledTerms$responsive = emails$responsive
library(caTools)
set.seed(1500)
spl = sample.split(labeledTerms$responsive, SplitRatio=0.75)
train = subset(labeledTerms, spl==TRUE)
test = subset(labeledTerms, spl==FALSE)
library(rpart)
library(rpart.plot)
textCART = rpart(responsive~.,data=train,method="class")
pred = predict(textCART, newdata = test)
pred.prob=pred[,2]
table(test$responsive, pred.prob>0.5)
(174+16)/(174+5+19+16)
#0.8878505
table(test$responsive, pred.prob>0.7
(174+16)/(174+5+19+16)
#0.8878505
table(test$responsive, pred.prob>0.7
(179)/(179+35)
#0.8364486
table(test$responsive, pred.prob>0.6)
0.8878505
table(test$responsive, pred.prob>0.8)





#Question-4:
climate = read.csv("climate_change.csv")
nrow(climate)
nrow(table(unique(climate$Year)))
library(ggplot2)
ggplot(climate, aes(x = N2O, y=Temp)) + geom_line(aes(group=1)) + xlab("Concentraion of N2O") + ylab("Temperature")
ggplot(climate, aes(x = Aerosols, y = Temp)) + geom_point(color='blue') + geom_smooth(method = "lm", se = FALSE,color="red")
ggplot(intl, aes(x=Region, y=PercentOfIntl)) +
geom_bar(stat="identity", fill="red") +
geom_text(aes(label=PercentOfIntl), hjust=1,angle=90) +
ylab("Percent of International Students") +
theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

#Question-5:

intl = read.csv("intl.csv")
ggplot()