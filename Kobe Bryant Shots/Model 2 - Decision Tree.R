library(dplyr)
library(ggplot2)
library(rpart)

#Set working directory
#setwd("~/Desktop/Kobe Bryant Shots")

#Read data file
rawData<-read.csv(file = "data.csv")


#Extract train and test data sets
trainData<-rawData%>%
  filter(!is.na(shot_made_flag))

testData<-rawData%>%
  filter(is.na(shot_made_flag))

fit <- rpart(shot_made_flag ~ combined_shot_type + loc_x+loc_y +
               minutes_remaining + period + playoffs + seconds_remaining +
               opponent, data=trainData, method="anova")

plot(fit)
text(fit)

library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)


prediction <- predict(fit, testData, type = "vector")
testData$shot_made_flag = prediction
submit <- data.frame(shot_id = testData$shot_id, shot_made_flag=testData$shot_made_flag)
write.csv(submit, file = "decisionTree1.csv", row.names = FALSE)

