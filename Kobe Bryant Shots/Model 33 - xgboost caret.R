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


#Select columns for model
reducedData<-rawData%>%
  select(action_type,
         combined_shot_type,
         loc_x,
         loc_y,
         minutes_remaining,
         period,
         playoffs,
         seconds_remaining,
         shot_distance,
         shot_made_flag,
         shot_type,
         shot_zone_area,
         shot_zone_basic,
         shot_zone_range,
         opponent,
         shot_id)

#Extract train and test data sets
trainData<-reducedData%>%
  filter(!is.na(shot_made_flag))

testData<-reducedData%>%
  filter(is.na(shot_made_flag))

xgbTreeFit<-train(shot_made_flag ~ .,
              data = trainData,
              method = 'xgbTree'
              )

pred <- predict(xgbTreeFit,testData,type='prob')
testData$shot_made_flag = pred[,2]
submit <- data.frame(shot_id = testData$shot_id, shot_made_flag=testData$shot_made_flag)
write.csv(submit, file = "Model 33 - xgboost caret.csv", row.names = FALSE)
