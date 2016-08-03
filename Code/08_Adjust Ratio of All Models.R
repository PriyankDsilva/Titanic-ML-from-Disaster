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
  prediction$TotalCount[i] <- print(as.numeric(prediction$H2ODeepLearning.prediction[i])  
  + as.numeric(prediction$H2OGBM.prediction[i])
  + as.numeric(prediction$H2ONaiveBayes.prediction[i])
  + as.numeric(prediction$H2ORandomForest.prediction[i])
  + as.numeric(prediction$H2OGLM.prediction[i])
  + as.numeric(prediction$RandomForest.prediction[i])
  + as.numeric(prediction$SVMModel.prediction[i])
  + as.numeric(prediction$GBM.prediction[i])
  + as.numeric(prediction$GLM.prediction[i])
  + as.numeric(prediction$NeuralNet.prediction[i])
  + as.numeric(prediction$Sofia.prediction[i]) )
}

#get final prediction
prediction$FinalPrediction <- 0  
#Code to having final prediction
for(i in seq(from=1,to=length(prediction$id))){
  if(prediction$TotalCount[i] >= 5){
    prediction$FinalPrediction[i] <- 1
  }
}


summary(as.factor(prediction$FinalPrediction))
True.Values <- filter(prediction, FinalPrediction == 1 )
False.Values <-  filter(prediction, FinalPrediction == 0 )
#length(True.Values$predict)
#length(False.Values$predict)
#length(H2OSecondLayerDeepLearning.prediction$predict)
Survived.Percentage <-print(c('Survived Percentage% ',length(True.Values$FinalPrediction)/length(prediction$FinalPrediction)))
Demise.Percentage <- print(c('DEAD Percentage% ',length(False.Values$FinalPrediction)/length(prediction$FinalPrediction)))

Survived.Percentage
Demise.Percentage

#colnames(prediction)
compare <- prediction[,c('PassengerId','Survived','FinalPrediction','TotalCount')]
Combine.prediction <- prediction[c(2,5,20,33,35),c('PassengerId','FinalPrediction','TotalCount',
  'H2ODeepLearning.prediction','H2OGBM.prediction','H2ONaiveBayes.prediction','H2ORandomForest.prediction'
                                    ,'H2OGLM.prediction','RandomForest.prediction','SVMModel.prediction','GBM.prediction',
                                    'GLM.prediction','NeuralNet.prediction','Sofia.prediction')]

# Save the predictions
submission.data <- data.frame(PassengerID = prediction$PassengerId, Survived = prediction$FinalPrediction)

# Write the solution to file
write.csv(submission.data, file = 'Submissions/11ModelAggrregate10PredictorsWithAdjustedRatio5.csv', row.names = F)
#write.csv(submission.data, file = 'H2oRandomForest_Predictions3.csv', row.names = F)





