#Set Working Environment
setwd('C:/Users/Priyank/OneDrive/Documents/MIS/Predictive Analysis/Titanic')

#Load data
load('RDFiles/TitanicManualImputation.Rd')

#Library
library(rpart)                                     
library(randomForest) 
library(class)
library(e1071)
library(plyr)

# Family ID for large families
training.data$FamilyID <- training.data$Family
training.data$FamilyID2 <- training.data$Family

training.data$FamilyID[training.data$FamilySize <= 2] <- 'Small'
training.data$FamilyID2[training.data$FamilySize <= 3] <- 'Small'

training.data$FamilyID <- as.factor(training.data$FamilyID)
training.data$FamilyID2 <- as.factor(training.data$FamilyID2)


#Seperate Data
testing.data <- training.data[892:1309,]
training.data <-training.data[1:891,]
colnames(training.data)


#Formula
predict_vars1 <- c('Pclass','BooleanSex0M1F','RoundedFillAge','RoundedFare','Embarked','FamilySize','SibSp','Parch')
predict_vars2 <- c('Pclass','BooleanSex0M1F','RoundedFillAge','RoundedFare','Embarked','FamilySize','SibSp','Parch')
predict_vars3 <- c('Pclass','BooleanSex0M1F','RoundedFillAge','RoundedFare','Embarked','FamilySize','SibSp','Parch')

f1 <- as.formula(paste(paste('Survived', collapse = " + "),'~', paste(predict_vars1, collapse = " + ")))
f2 <- as.formula(paste(paste('Survived', collapse = " + "),'~', paste(predict_vars2, collapse = " + ")))
f3 <- as.formula(paste(paste('Survived', collapse = " + "),'~', paste(predict_vars3, collapse = " + ")))


## Decision Tree Model
RPartModel <- rpart(f1, data=training.data, method="class")
RPartModelPrediction <- predict(RPartModel, testing.data, type = "class")

# Random Forest Model
RandomForestModel <- randomForest(f2, data=training.data, importance=T, ntree=2000)
RandomForestModelPrediction <- predict(RandomForestModel, testing.data, type = "class")

# SVM Model
SVMModel <- svm(f3, data = training.data)
SVMModelPrediction <- predict(SVMModel, testing.data, type = "class")

# Ensemble Model (RPart, Random Forest, SVM )
prediction <- RandomForestModelPrediction
prediction[RandomForestModelPrediction != SVMModelPrediction & RandomForestModelPrediction != RPartModelPrediction & prediction == 0] <- 1
prediction[RandomForestModelPrediction != SVMModelPrediction & RandomForestModelPrediction != RPartModelPrediction & prediction == 1] <- 0

# Save the predictions
submission.data <- data.frame(PassengerID = testing.data$PassengerId, Survived = prediction)

summary(submission.data$Survived)

# Write the solution to file
write.csv(submission.data, file = 'Submissions/Ensemble3Model5.csv', row.names = F)

