#Set Working Environment
setwd('C:/Users/Priyank/OneDrive/Documents/MIS/Predictive Analysis/Titanic')

#Load data
#load('RDFiles/H2oAndOtherMLPredictions_HalfTraining.Rd')
load('RDFiles/H2oAndOtherMLPredictions_ForSecondModel.Rd')

summary(prediction)


#Factor Variables
factor_vars <- c('H2ODeepLearning.prediction','H2OGBM.prediction','H2ONaiveBayes.prediction','H2ORandomForest.prediction'
                 ,'H2OGLM.prediction','RandomForest.prediction','SVMModel.prediction','GBM.prediction',
                 'GLM.prediction','NeuralNet.prediction','Sofia.prediction','Survived')
prediction[factor_vars] <- lapply(prediction[factor_vars], function(x) as.factor(x))
summary(prediction)
prediction[factor_vars] <- lapply(prediction[factor_vars], function(x) as.integer(as.character(x)))
summary(prediction)


for(i in seq(from=1,to=length(prediction$id))){
  if(prediction$Survived[i] == prediction$H2ODeepLearning.prediction[i] ){
    prediction$H2ODeepLearning.prediction[i] <- 'Match'
  }else{prediction$H2ODeepLearning.prediction[i] <- 'Not-Match'}
  if(prediction$Survived[i] == prediction$H2OGBM.prediction[i] ){
    prediction$H2OGBM.prediction[i] <- 'Match'
  }else{prediction$H2OGBM.prediction[i] <- 'Not-Match'}
  if(prediction$Survived[i] == prediction$H2ONaiveBayes.prediction[i] ){
    prediction$H2ONaiveBayes.prediction[i] <- 'Match'
  }else{prediction$H2ONaiveBayes.prediction[i] <- 'Not-Match'}
  if(prediction$Survived[i] == prediction$H2ORandomForest.prediction[i] ){
    prediction$H2ORandomForest.prediction[i] <- 'Match'
  }else{prediction$H2ORandomForest.prediction[i] <- 'Not-Match'}
  if(prediction$Survived[i] == prediction$H2OGLM.prediction[i] ){
    prediction$H2OGLM.prediction[i] <- 'Match'
  }else{prediction$H2OGLM.prediction[i] <- 'Not-Match'}
  if(prediction$Survived[i] == prediction$RandomForest.prediction[i] ){
    prediction$RandomForest.prediction[i] <- 'Match'
  }else{prediction$RandomForest.prediction[i] <- 'Not-Match'}
  if(prediction$Survived[i] == prediction$SVMModel.prediction[i] ){
    prediction$SVMModel.prediction[i] <- 'Match'
  }else{prediction$SVMModel.prediction[i] <- 'Not-Match'}
  if(prediction$Survived[i] == prediction$GBM.prediction[i] ){
    prediction$GBM.prediction[i] <- 'Match'
  }else{prediction$GBM.prediction[i] <- 'Not-Match'}
  if(prediction$Survived[i] == prediction$GLM.prediction[i] ){
    prediction$GLM.prediction[i] <- 'Match'
  }else{prediction$GLM.prediction[i] <- 'Not-Match'}
  if(prediction$Survived[i] == prediction$NeuralNet.prediction[i] ){
    prediction$NeuralNet.prediction[i] <- 'Match'
  }else{prediction$NeuralNet.prediction[i] <- 'Not-Match'}
  if(prediction$Survived[i] == prediction$Sofia.prediction[i] ){
    prediction$Sofia.prediction[i] <- 'Match'
  }else{prediction$Sofia.prediction[i] <- 'Not-Match'}
}


prediction[factor_vars] <- lapply(prediction[factor_vars], function(x) as.factor(x))
summary(prediction)


H2ODeepLearning.prediction <- dim(filter(prediction, H2ODeepLearning.prediction == 'Not-Match' ))[1]
H2OGBM.prediction <- dim(filter(prediction, H2OGBM.prediction == 'Not-Match' ))[1]
H2ONaiveBayes.prediction <- dim(filter(prediction, H2ONaiveBayes.prediction == 'Not-Match' ))[1]
H2ORandomForest.prediction <- dim(filter(prediction, H2ORandomForest.prediction == 'Not-Match' ))[1]
H2OGLM.prediction <- dim(filter(prediction, H2OGLM.prediction == 'Not-Match' ))[1]
RandomForest.prediction <- dim(filter(prediction, RandomForest.prediction == 'Not-Match' ))[1]
SVMModel.prediction <- dim(filter(prediction, SVMModel.prediction == 'Not-Match' ))[1]
GBM.prediction <- dim(filter(prediction, GBM.prediction == 'Not-Match' ))[1]
GLM.prediction <- dim(filter(prediction, GLM.prediction == 'Not-Match' ))[1]
NeuralNet.prediction <- dim(filter(prediction, NeuralNet.prediction == 'Not-Match' ))[1]
Sofia.prediction <- dim(filter(prediction, Sofia.prediction == 'Not-Match' ))[1]

Count.Table <- c('H2ODeepLearning.prediction',H2ODeepLearning.prediction)
Count.Table <- rbind(Count.Table,c('H2OGBM.prediction',H2OGBM.prediction))
Count.Table <- rbind(Count.Table,c('H2ONaiveBayes.prediction',H2ONaiveBayes.prediction))
Count.Table <- rbind(Count.Table,c('H2ORandomForest.prediction',H2ORandomForest.prediction))
Count.Table <- rbind(Count.Table,c('H2OGLM.prediction',H2OGLM.prediction))
Count.Table <- rbind(Count.Table,c('RandomForest.prediction',RandomForest.prediction))
Count.Table <- rbind(Count.Table,c('SVMModel.prediction',SVMModel.prediction))
Count.Table <- rbind(Count.Table,c('GBM.prediction',GBM.prediction))
Count.Table <- rbind(Count.Table,c('GLM.prediction',GLM.prediction))
Count.Table <- rbind(Count.Table,c('NeuralNet.prediction',NeuralNet.prediction))
Count.Table <- rbind(Count.Table,c('Sofia.prediction',Sofia.prediction))

Count.Table


summary(prediction[,c('H2ODeepLearning.prediction','H2OGBM.prediction','H2ONaiveBayes.prediction','H2ORandomForest.prediction'
                        ,'H2OGLM.prediction','RandomForest.prediction','SVMModel.prediction','GBM.prediction',
                        'GLM.prediction','NeuralNet.prediction','Sofia.prediction')])
#RandomForest.prediction
#SVMModel.prediction
#GLM.prediction


