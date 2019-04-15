
songs = read.csv("songs.csv")
str(songs)
table(songs$year)

table(songs$artistname == "Michael Jackson")
MJ = subset(songs, artistname == "Michael Jackson")
str(MJ)


MJ[c("songtitle", "Top10")]


table(songs$timesignature)


songs$songtitle[which.max(songs$tempo)]



SongsTrain = subset(songs, year <= 2009)
SongsTest = subset(songs, year == 2010)
str(SongsTrain)


nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]
SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog1)


cor(SongsTrain$loudness, SongsTrain$energy)


SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)

summary(SongsLog2)



SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)




pred3 = predict(SongsLog3, newdata=SongsTest, type="response")
table(SongsTest$Top10, pred3 >= 0.45)
(309+19)/(309+5+40+19)


table(SongsTest$Top10)
314/(309+5+40+19)


table(SongsTest$Top10, pred3 >= 0.45)


19/(40+19)

309/(309+5)

