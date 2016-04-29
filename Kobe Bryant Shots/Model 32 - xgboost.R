#http://xgboost.readthedocs.io/en/latest/R-package/discoverYourData.html
#http://www.analyticsvidhya.com/blog/2016/01/xgboost-algorithm-easy-steps/
#https://github.com/dmlc/xgboost/blob/master/R-package/vignettes/xgboostPresentation.Rmd


library(dplyr)
library(ggplot2)
library(tidyr)
library(xgboost)
library(Matrix)

#Set working directory
setwd("C:/Users/joet/Desktop/[CURRENT]/GitHub/Kaggle/Kobe Bryant Shots")

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
  select(action_type_2,
         secondsElapsedMatch,
         loc_x,
         seconds_remaining,
         secondsRemainPeriod,
         loc_y,
         shot_distance,
         minutes_remaining,
         combined_shot_type,
         location,
         playoffs,
         season,
         opponent,
         opponentConference,
         shot_made_flag,
         shot_id)


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

############################
#Convert factors to numeric#
############################

#Grab shot_made_flags
trainShotMadeFlags<-trainData$shot_made_flag
testShotMadeFlags<-testData$shot_made_flag
testData$shot_made_flag<-1

#-1 is required to remove additional column that is created but th function
sparse_matrix_train <- sparse.model.matrix(shot_made_flag~.-1, data=trainData)

sparse_matrix_test <- sparse.model.matrix(shot_made_flag~.-1, data=testData) 


#Fit and examine model
xgboostfit1 <- xgboost(data = sparse_matrix_train,
                       label = trainShotMadeFlags,
                       nrounds = 10,
                       objective = "binary:logistic",
                       set.seed=1202)


importance <- xgb.importance(feature_names = sparse_matrix_train@Dimnames[[2]], model = xgboostfit1)
importance
xgb.plot.importance(importance_matrix = importance)

dtrain <- xgb.DMatrix(sparse_matrix_train, label = trainShotMadeFlags)
history <- xgb.cv(data = dtrain, nround=10, nfold = 5, early.stop.round = 3, metrics=list("rmse","auc"),
                  objective = "binary:logistic")


pred <- predict(xgboostfit1,sparse_matrix_test)
testData$shot_made_flag = pred
submit <- data.frame(shot_id = testShotIDs, shot_made_flag=pred)
write.csv(submit, file = "Model 32 - xgboost.csv", row.names = FALSE)
