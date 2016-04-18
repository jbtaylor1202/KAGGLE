library(dplyr)
library(ggplot2)
library(randomForest)

#Set working directory
setwd("~/Desktop/Kobe Bryant Shots")

set.seed(2012)

#Read data file
rawData<-read.csv(file = "data.csv")


#Extract train and test data sets
trainData<-rawData%>%
  filter(!is.na(shot_made_flag))

trainData$matchup<-substr(trainData$matchup,5,5)

testData<-rawData%>%
  filter(is.na(shot_made_flag))

testData$matchup<-substr(testData$matchup,5,5)

trainData$game_date<-unclass(trainData$game_date)
trainData$action_type<-unclass(trainData$action_type)

fit<-randomForest(shot_made_flag ~ shot_distance + shot_zone_basic, data=trainData, importance = TRUE, ntree = 2000)

varImpPlot(fit)

prediction <- predict(fit, testData)
testData$shot_made_flag = prediction
submit <- data.frame(shot_id = testData$shot_id, shot_made_flag=testData$shot_made_flag)
write.csv(submit, file = "randomForest1.csv", row.names = FALSE)
