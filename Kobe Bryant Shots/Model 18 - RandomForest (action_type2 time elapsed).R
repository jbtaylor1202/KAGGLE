library(dplyr)
library(randomForest)

#Set working directory
setwd("C:/Users/joet/Desktop/[CURRENT]/GitHub/Kaggle/Kobe Bryant Shots")

set.seed(2012)

#Read data file
rawData<-read.csv(file = "data.csv")

#summarize action_type, remove levels where less than 30 shots occurred
action_type_rare <- rawData %>% 
  select(action_type) %>% 
  group_by(action_type) %>% 
  summarise(count=n()) %>% 
  filter(count < 30) %>% 
  select(action_type) %>% 
  droplevels

#store as vector
action_type_rare <- action_type_rare$action_type

#grab action_type vector
action_type <- rawData$action_type

#create new factor with fewer levels and 'Rare Shot' level
action_type_2 <- factor(ifelse(action_type %in% action_type_rare,0,action_type), labels=c('Rare Shot',levels(action_type)[!levels(action_type) %in% levels(action_type_rare)]))

#add new level to data
rawData <- cbind(rawData,action_type_2)


rawData$matchup<-substr(rawData$matchup,5,5)
rawData$matchup[rawData$matchup=='@']<-"Away"
rawData$matchup[rawData$matchup=='v']<-"Home"
rawData$matchup<-as.factor(rawData$matchup)

rawData$timeLeft<-(rawData$minutes_remaining*60)+rawData$seconds_remaining
rawData$timeElapsed<-(12*60)-rawData$time

#Select final columns
rawDataPreped<-rawData%>%
  select(-game_event_id,-game_id,-team_id,-team_name,-game_date)

#Extract train and test data sets
trainData<-rawDataPreped%>%
  filter(!is.na(shot_made_flag))

#Extract shot id and then remove
trainShotId<-trainData$shot_id
trainData<-trainData%>%
  select(-shot_id, -action_type, -minutes_remaining, -seconds_remaining)

testData<-rawDataPreped%>%
  filter(is.na(shot_made_flag))

#Extract shot id and then remove
testShotId<-testData$shot_id
testData<-testData%>%
  select(-shot_id, -action_type, -minutes_remaining, -seconds_remaining)

#Build forest with all variables
fit<-randomForest(shot_made_flag ~ action_type_2+ timeElapsed,
                  data=trainData, importance = TRUE, ntree = 2000)

#Check MSE and importance of variables
fit
varImpPlot(fit)
importance(fit)
which.min(fit$mse)

#Run prediction
prediction <- predict(fit, testData)
testData$shot_made_flag = prediction

#Write prediction for submission
submit <- data.frame(shot_id = testShotId, shot_made_flag=testData$shot_made_flag)
write.csv(submit, file = "Model 18 - RandomForest (action_type2 time elapsed).csv", row.names = FALSE)

