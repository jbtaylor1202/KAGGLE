library(caret)
library(xgboost)
library(plyr)
library(dplyr)

#Set working directory
setwd("C:/Users/joet/Desktop/[CURRENT]/GitHub/Kaggle/Kobe Bryant Shots")

#Read data file
rawData<-read.csv(file = "data.csv")

rawData$shot_made_flag[rawData$shot_made_flag==0]<-'Miss'
rawData$shot_made_flag[rawData$shot_made_flag==1]<-'Score'
rawData$shot_made_flag<-as.factor(rawData$shot_made_flag)


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

#Sort minutes
rawData$period[rawData$period>4] <- '4'
rawData$period<-as.numeric(rawData$period)
rawData$secondsRemainPeriod<-(rawData$minutes_remaining*60)+(rawData$seconds_remaining)
rawData$secondsElapsedPeriod<-(12*60)-rawData$secondsRemainPeriod
rawData$secondsElapsedMatch[rawData$period==1] <-rawData$secondsElapsedPeriod 
rawData$secondsElapsedMatch[rawData$period==2] <-(12*60)+rawData$secondsElapsedPeriod 
rawData$secondsElapsedMatch[rawData$period==3] <-(24*60)+rawData$secondsElapsedPeriod
rawData$secondsElapsedMatch[rawData$period==4] <-(36*60)+rawData$secondsElapsedPeriod


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

#Select columns for model
reducedData<-rawData%>%
  select(-action_type,
         -game_event_id, 
         -game_id,
         -team_id,
         -team_name,
         -game_date,
         -season,
         -matchup)

#Add factors where appropriate
reducedData$period<-as.factor(reducedData$period)
reducedData$playoffs<-as.factor(reducedData$playoffs)



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


xgbTreeFit<-train(shot_made_flag ~ .,
                  data = trainData,
                  method = 'xgbTree',
                  tuneLength = 10
)

pred <- predict(xgbTreeFit,testData,type='prob')
testData$shot_made_flag = pred[,2]
submit <- data.frame(shot_id = testShotIDs, shot_made_flag=testData$shot_made_flag)
write.csv(submit, file = "Model 35 - xgboost caret.csv", row.names = FALSE)
