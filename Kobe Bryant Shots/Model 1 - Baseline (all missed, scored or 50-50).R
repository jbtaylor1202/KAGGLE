library(dplyr)
library(ggplot2)

#Set working directory
#setwd("~/Desktop/Kobe Bryant Shots")

#Read data file
rawData<-read.csv(file = "data.csv")


#Extract train and test data sets
trainData<-rawData%>%
  filter(!is.na(shot_made_flag))
  
testData<-rawData%>%
  filter(is.na(shot_made_flag))

#Check for most common outcome of shot in training set
table(trainData$shot_made_flag)
prop.table(table(trainData$shot_made_flag))

#Add most common outcome (i.e. 0) to the shot_made_flag
testData$shot_made_flag = 0

#Create csv submission
submit <- data.frame(shot_id = testData$shot_id, shot_made_flag=testData$shot_made_flag)
write.csv(submit, file = "allShotsMiss.csv", row.names = FALSE)


###################
#All Score
#Add most least outcome (i.e. 1) to the shot_made_flag
testData$shot_made_flag = 1

#Create csv submission
submit2 <- data.frame(shot_id = testData$shot_id, shot_made_flag=testData$shot_made_flag)
write.csv(submit2, file = "allShotsScore.csv", row.names = FALSE)


#All 50/50
#Add most 50/50 chance (0.5) to the shot_made_flag
testData$shot_made_flag = 0.5

#Create csv submission
submit3<- data.frame(shot_id = testData$shot_id, shot_made_flag=testData$shot_made_flag)
write.csv(submit3, file = "allShots50.csv", row.names = FALSE)