#Set Working Environment
setwd('C:/Users/Priyank/OneDrive/Documents/MIS/Predictive Analysis/Titanic')

#Load data
load('RDFiles/H2oAndOtherMLPredictions_FullTraining.Rd')
#load('RDFiles/H2oAndOtherMLPredictions_ForSecondModel.Rd')


#Factor Variables
factor_vars <- c('H2ODeepLearning.prediction','H2OGBM.prediction','H2ONaiveBayes.prediction','H2ORandomForest.prediction'
                 ,'H2OGLM.prediction','RandomForest.prediction','SVMModel.prediction','GBM.prediction',
                 'GLM.prediction','NeuralNet.prediction','Sofia.prediction')
prediction[factor_vars] <- lapply(prediction[factor_vars], function(x) as.factor(x))
summary(prediction)
prediction[factor_vars] <- lapply(prediction[factor_vars], function(x) as.integer(as.character(x)))
summary(prediction)

#Total Predictions Count
prediction$TotalCount <- NULL
#Code to add all predictions
for(i in seq(from=1,to=length(prediction$id))){
  prediction$TotalCount[i] <- print(as.numeric(prediction$H2ORandomForest.prediction[i])  
  + as.numeric(prediction$H2OGBM.prediction[i])
  + as.numeric(prediction$H2ODeepLearning.prediction[i]))
}

#get final prediction
prediction$FinalPrediction <- 0  
#Code to having final prediction
for(i in seq(from=1,to=length(prediction$id))){
  if(prediction$TotalCount[i] >= 3){
    prediction$FinalPrediction[i] <- 1
  }
}


summary(as.factor(prediction$FinalPrediction))
True.Values <- filter(prediction, FinalPrediction == 1 )
False.Values <-  filter(prediction, FinalPrediction == 0 )
#length(True.Values$predict)
#length(False.Values$predict)
#length(H2OSecondLayerDeepLearning.prediction$predict)
print(c('Survived Percentage% ',length(True.Values$FinalPrediction)/length(prediction$FinalPrediction)))
print(c('DEAD Percentage% ',length(False.Values$FinalPrediction)/length(prediction$FinalPrediction)))


colnames(prediction)
Combine.prediction <- prediction[,c('PassengerId','FinalPrediction','TotalCount',
  'H2ORandomForest.prediction','H2OGBM.prediction','H2ODeepLearning.prediction')]

# Save the predictions
submission.data <- data.frame(PassengerID = prediction$PassengerId, Survived = prediction$FinalPrediction)

# Write the solution to file
write.csv(submission.data, file = 'Submissions/3ModelAggrregate10Predictors.csv', row.names = F)
#write.csv(submission.data, file = 'H2oRandomForest_Predictions3.csv', row.names = F)





