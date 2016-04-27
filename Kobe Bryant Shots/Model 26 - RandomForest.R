library(dplyr)
library(ggplot2)
library(tidyr)
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
rawData$location[rawData$matchup == "@"] <- 'Away'
rawData$location[rawData$matchup == "v"] <- 'Home'
rawData$location<-as.factor(rawData$location)

#Adjust periods and add new feature
rawData$period[rawData$period>4] <- '4'
rawData$period<-as.numeric(rawData$period)
rawData$periodAddedFeature[rawData$period<4] <- 0
rawData$periodAddedFeature[rawData$period==4] <- 1

#Sort minutes
rawData$secondsRemainPeriod<-(rawData$minutes_remaining*60)+(rawData$seconds_remaining)
rawData$secondsElapsedPeriod<-(12*60)-rawData$secondsRemainPeriod
rawData$secondsElapsedMatch[rawData$period==1] <-rawData$secondsElapsedPeriod 
rawData$secondsElapsedMatch[rawData$period==2] <-(12*60)+rawData$secondsElapsedPeriod
rawData$secondsElapsedMatch[rawData$period==3] <-(24*60)+rawData$secondsElapsedPeriod
rawData$secondsElapsedMatch[rawData$period==4] <-(36*60)+rawData$secondsElapsedPeriod

#Add new feature for last minute and last second
rawData$minutesRemainingAddedFeature[rawData$minutes_remaining<=1] <- 1
rawData$minutesRemainingAddedFeature[rawData$minutes_remaining>1] <- 0
rawData$secondsRemainingAddedFeature[rawData$seconds_remaining<=5] <- 1
rawData$secondsRemainingAddedFeature[rawData$seconds_remaining>5] <- 0

#Add Division column with teams
rawData$opponentConference[rawData$opponent=='ATL']<-'East'
rawData$opponentConference[rawData$opponent=='BKN']<-'East'
rawData$opponentConference[rawData$opponent=='BOS']<-'East'
rawData$opponentConference[rawData$opponent=='CHA']<-'East'
rawData$opponentConference[rawData$opponent=='CHI']<-'East'
rawData$opponentConference[rawData$opponent=='CLE']<-'East'
rawData$opponentConference[rawData$opponent=='DAL']<-'West'
rawData$opponentConference[rawData$opponent=='DEN']<-'West'
rawData$opponentConference[rawData$opponent=='DET']<-'East'
rawData$opponentConference[rawData$opponent=='GSW']<-'West'
rawData$opponentConference[rawData$opponent=='HOU']<-'West'
rawData$opponentConference[rawData$opponent=='IND']<-'East'
rawData$opponentConference[rawData$opponent=='LAC']<-'West'
rawData$opponentConference[rawData$opponent=='MEM']<-'West'
rawData$opponentConference[rawData$opponent=='MIA']<-'East'
rawData$opponentConference[rawData$opponent=='MIL']<-'East'
rawData$opponentConference[rawData$opponent=='MIN']<-'West'
rawData$opponentConference[rawData$opponent=='NJN']<-'East'
rawData$opponentConference[rawData$opponent=='NOH']<-'West'
rawData$opponentConference[rawData$opponent=='NOP']<-'West'
rawData$opponentConference[rawData$opponent=='NYK']<-'East'
rawData$opponentConference[rawData$opponent=='OKC']<-'West'
rawData$opponentConference[rawData$opponent=='ORL']<-'East'
rawData$opponentConference[rawData$opponent=='PHI']<-'East'
rawData$opponentConference[rawData$opponent=='PHX']<-'West'
rawData$opponentConference[rawData$opponent=='POR']<-'West'
rawData$opponentConference[rawData$opponent=='SAC']<-'West'
rawData$opponentConference[rawData$opponent=='SAS']<-'West'
rawData$opponentConference[rawData$opponent=='SEA']<-'West'
rawData$opponentConference[rawData$opponent=='TOR']<-'East'
rawData$opponentConference[rawData$opponent=='UTA']<-'West'
rawData$opponentConference[rawData$opponent=='VAN']<-'West'
rawData$opponentConference[rawData$opponent=='WAS']<-'East'
rawData$opponentConference<-as.factor(rawData$opponentConference)

#Correct a couple of teams
#New Jersy Jest Became Broklyn
rawData$opponent[rawData$opponent=='NJN']<-'BKN'
#NOP is NOH
rawData$opponent[rawData$opponent=='NOP']<-'NOH'
#Seattle did become Oklahoma but left alone given distance between cities

#Remove non-useful columns
reducedData<-rawData%>%
  select(-action_type,
         -game_event_id, 
         -game_id,
         -team_id,
         -team_name,
         -game_date,
         -lat,
         -lon,
         -loc_x,
         -loc_y,
         -matchup)

#Extract train and test data sets
trainData<-reducedData%>%
  filter(!is.na(shot_made_flag))

testData<-reducedData%>%
  filter(is.na(shot_made_flag))

#grab shot ids
trainShotIDs<-trainData$shot_id
testShotIDs<-testData$shot_id

#Remove shot_ids
trainData<-trainData%>%
  select(-shot_id)

testData<-testData%>%
  select(-shot_id)

#Fit and examine model
fit<-randomForest(shot_made_flag ~ action_type_2
                  + secondsRemainPeriod+shot_distance, 
                  data=trainData, importance = TRUE, ntree = 2000)

fit
summary(fit)
importance(fit)
varImpPlot(fit)
fit$importance
fit$test
fit$mse

#Tune Forest
#tuneRF(x, y, mtryStart, ntreeTry=50, stepFactor=2, improve=0.05,
#       trace=TRUE, plot=TRUE, doBest=FALSE, ...)

prediction <- predict(fit, testData, type = "response")
testData$shot_made_flag = prediction
submit <- data.frame(shot_id = testShotIDs, shot_made_flag=prediction)
write.csv(submit, file = "Model 26 - RandomForest.csv", row.names = FALSE)
