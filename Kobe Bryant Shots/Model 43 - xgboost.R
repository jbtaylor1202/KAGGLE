#Based on the script submitted by Humberto Brandão:
#https://www.kaggle.com/brandao/kobe-bryant-shot-selection/xgboost-in-r-kobe-bryant-benchmark

library(xgboost)
library(data.table)
library(Matrix)
library(doParallel)

#Set working directory
setwd("C:/Users/joet/Desktop/[CURRENT]/GitHub/Kaggle/Kobe Bryant Shots")

#Read data file
rawData<-read.csv(file = "data.csv")

#New Features1
rawData$matchup<-substr(rawData$matchup,5,5)
rawData$location[rawData$matchup == "@"] <- 'Away'
rawData$location[rawData$matchup == "v"] <- 'Home'
rawData$location<-as.factor(rawData$location)

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


#Split data file
train<-subset(rawData, !is.na(rawData$shot_made_flag))
test<-subset(rawData, is.na(rawData$shot_made_flag))

test.id <- test$shot_id
train$shot_id <- NULL
test$shot_id <- NULL

#New Features2
train$time_remaining <- train$minutes_remaining*60+train$seconds_remaining
test$time_remaining <- test$minutes_remaining*60+test$seconds_remaining
train$shot_distance[train$shot_distance>45] <- 45
test$shot_distance[test$shot_distance>45] <- 45

#Dropping features
train$seconds_remaining<-NULL
test$seconds_remaining<-NULL
train$team_name <- NULL
test$team_name <- NULL
train$team_id <- NULL
test$team_id <- NULL
train$game_event_id <- NULL
test$game_event_id <- NULL
train$game_id <- NULL
test$game_id <- NULL
train$lat <- NULL
test$lat <- NULL
train$lon <- NULL
test$lon <- NULL

train.y = train$shot_made_flag

train$shot_made_flag <- NULL
test$shot_made_flag <- NULL

pred <- rep(0,nrow(test))

#Creating data.matrix
trainM<-data.matrix(train, rownames.force = NA)
#Creating DMarix for xgboost
dtrain <- xgb.DMatrix(data=trainM, label=train.y, missing = NaN)

watchlist <- list(trainM=dtrain)

set.seed(1202)

param <- list(  objective           = "binary:logistic", 
                booster             = "gbtree",
                eval_metric         = "logloss",
                eta                 = 0.035,
                max_depth           = 4,
                subsample           = 0.40,
                colsample_bytree    = 0.40
)

clf <- xgb.cv(  params              = param, 
                data                = dtrain, 
                nrounds             = 1500, 
                verbose             = 1,
                watchlist           = watchlist,
                maximize            = FALSE,
                nfold               = 3,
                early.stop.round    = 10,
                print.every.n       = 1
)


bestRound <- which.min( as.matrix(clf)[,3] )
cat("Best round:", bestRound,"\n")
cat("Best result:",min(as.matrix(clf)[,3]),"\n")


cl<-makeCluster(6)
registerDoParallel(cl)


clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = bestRound, 
                    verbose             = 1,
                    watchlist           = watchlist,
                    maximize            = FALSE
)

stopCluster(cl)

testM <-data.matrix(test, rownames.force = NA)
preds <- predict(clf, testM)

summary(clf)
clf
save(clf,file = "Model43.RData")

submit <- data.frame(shot_id=test.id, shot_made_flag=preds)
write.csv(submit, file = "Model 43 - xgboost.csv", row.names = FALSE)
