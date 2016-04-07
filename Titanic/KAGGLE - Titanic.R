#http://trevorstephens.com/post/72923766261/titanic-getting-started-with-r-part-3-decision

setwd("~/Desktop")

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

train<-read.csv(file = 'train.csv')
test<-read.csv(file = 'test.csv')

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")
fancyRpartPlot(fit)


Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)


fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train,
             method="class", control=rpart.control(minsplit=10))
new.fit <- prp(fit,snip=TRUE)$obj
fancyRpartPlot(new.fit)

##############Lesson2
test$Survived <- NA
combi <- rbind(train, test)
combi$Name <- as.character(combi$Name)
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
table(combi$Title)
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title <- factor(combi$Title)
combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
table(combi$FamilyID)
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)
train <- combi[1:891,]
test <- combi[892:1309,]

fit2 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
  data=train, method="class")

Prediction2 <- predict(fit2, test,type = "class")
submit2 <- data.frame(PassengerId = test$PassengerId, Survived = Prediction2)
write.csv(submit2, file = "engineeredVariables.csv", row.names = FALSE)



new.fit2 <- prp(fit2,snip=TRUE)$obj
fancyRpartPlot(new.fit2)

#############Lesson3
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
which(combi$Embarked == '')
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)
which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

library(randomForest)
train <- combi[1:891,]
test <- combi[892:1309,]

set.seed(415)
fit3 <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize +
                      FamilyID2, data=train, importance=TRUE, ntree=2000)
varImpPlot(fit3)
Prediction3 <- predict(fit3, test)
submit3 <- data.frame(PassengerId = test$PassengerId, Survived = Prediction3)
write.csv(submit3, file = "firstforest.csv", row.names = FALSE)



library(party)
set.seed(415)
fit4 <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
prettytree(fit4)
Prediction4 <- predict(fit4, test, OOB=TRUE, type = "response")
submit4 <- data.frame(PassengerId = test$PassengerId, Survived = Prediction4)
write.csv(submit4, file = "condiitonalForest.csv", row.names = FALSE)



##########
setwd("~/Desktop")

library(rpart)

train<-read.csv(file = 'train.csv')
test<-read.csv(file = 'test.csv')

test$Survived <- NA
combi <- rbind(train, test)
combi$Name <- as.character(combi$Name)
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
table(combi$Title)
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title <- factor(combi$Title)
combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
table(combi$FamilyID)
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
which(combi$Embarked == '')
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)
which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

train <- combi[1:891,]
test <- combi[892:1309,]

train$Survived<-as.factor(train$Survived)
test$Survived<-as.factor(test$Survived)

logModel1<-glm(Survived~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title,data = train,family = binomial)
logModel2<-step(logModel1)

LogModel1Prediction <- predict(logModel1, test, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = LogModel1Prediction)


LogModel2Prediction <- predict(logModel2, test, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = LogModel2Prediction)

submit$Survived[submit$Survived >=0.5] <- 1
submit$Survived[ submit$Survived != 1] <- 0
submit$Survived[is.na(submit$Survived)] <- 0

write.csv(submit, file = "logModel1.csv", row.names = FALSE)
write.csv(submit, file = "logModel2.csv", row.names = FALSE)
