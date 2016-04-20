library(dplyr)
library(ggplot2)
library(randomForest)

#Set working directory
setwd("C:/Users/joet/Desktop/[CURRENT]/GitHub/Kaggle/Kobe Bryant Shots")

set.seed(2012)

#Read data file
rawData<-read.csv(file = "data.csv")

#Prepare data file
rawData$action_type<-unclass(rawData$action_type)
rawData$game_date<-unclass(rawData$game_date)

rawData$matchup<-substr(rawData$matchup,5,5)
rawData$matchup[rawData$matchup=='@']<-"Away"
rawData$matchup[rawData$matchup=='v']<-"Home"
rawData$matchup<-as.factor(rawData$matchup)

#Select final columns
rawDataPreped<-rawData%>%
  select(-game_event_id,-game_id,-team_id,-team_name,-game_date)

#Extract train and test data sets
trainData<-rawDataPreped%>%
  filter(!is.na(shot_made_flag))

#Extract shot id and then remove
trainShotId<-trainData$shot_id
trainData<-trainData%>%
  select(-shot_id)

testData<-rawDataPreped%>%
  filter(is.na(shot_made_flag))

#Extract shot id and then remove
testShotId<-testData$shot_id
testData<-testData%>%
  select(-shot_id)

#Build forest with all variables
fit<-randomForest(shot_made_flag ~ action_type + combined_shot_type,
                  data=trainData, importance = TRUE, ntree = 2000)

#Check MSE and importance of variables
fit
varImpPlot(fit)
importance(fit)
which.min(fit$mse)

#trainingInput<-trainData%>%select(-shot_made_flag)
#trainingOutput<-trainData$shot_made_flag
#result <- rfcv(trainingInput, trainingOutput, cv.fold=10)

#Run prediction
prediction <- predict(fit, testData)
testData$shot_made_flag = prediction

#RMSE.forest<-sqrt(mean((prediction-testData$shot_made_flag)^2))
#RMSE.forest
#MAE.forest<-mean(abs(prediction-testData$shot_made_flag))
#MAE.forest

#Write prediction for submission
submit <- data.frame(shot_id = testShotId, shot_made_flag=testData$shot_made_flag)
write.csv(submit, file = "Model 10 - RandomForest.csv", row.names = FALSE)
