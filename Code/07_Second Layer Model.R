#Set Working Environment
setwd('C:/Users/Priyank/OneDrive/Documents/MIS/Predictive Analysis/Titanic')

#Load data
load('RDFiles/H2oAndOtherMLPredictions_ForSecondModel.Rd')
training.data <- prediction
load('RDFiles/H2oAndOtherMLPredictions_FullTraining.Rd')
testing.data <- prediction

#Factor Variables
factor_vars <- c('H2ODeepLearning.prediction','H2OGBM.prediction','H2ONaiveBayes.prediction','H2ORandomForest.prediction'
                 ,'H2OGLM.prediction','RandomForest.prediction','SVMModel.prediction','GBM.prediction',
                 'GLM.prediction','NeuralNet.prediction','Sofia.prediction')
prediction[factor_vars] <- lapply(prediction[factor_vars], function(x) as.factor(x))
summary(prediction)


# Start h2o ,decide min and max memory sizes,number of cpu cores to be used (-1 means all cores)
localH2O <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE,
                     max_mem_size = '12g', min_mem_size = '4g', nthreads = -1)



#Training data
training.data.hex<-as.h2o(localH2O, training.data)

#crete H2O model
H2OSecondLayerDeepLearning <- h2o.deeplearning(x = c(4:length(training.data)), y =3 , 
                                               training_frame = training.data.hex,
                                               activation = 'RectifierWithDropout',
                                               loss = 'MeanSquare',
                                               overwrite_with_best_model = TRUE,
                                               hidden = c(1000,1000),
                                               model_id='H2OSecondLayerDeepLearning', epochs = 1000)


#predict values for the Testing dataset 
testing.data.hex<-as.h2o(localH2O, testing.data[,c(4:(length(training.data)))])


#H2ODeepLearning
H2OSecondLayerDeepLearning.prediction.hex <- h2o.predict(H2OSecondLayerDeepLearning, testing.data.hex)
H2OSecondLayerDeepLearning.prediction <- as.data.frame(H2OSecondLayerDeepLearning.prediction.hex)

summary(as.factor(H2OSecondLayerDeepLearning.prediction$predict))
True.Values <- filter(H2OSecondLayerDeepLearning.prediction, predict == 1 )
False.Values <-  filter(H2OSecondLayerDeepLearning.prediction, predict == 0 )
#length(True.Values$predict)
#length(False.Values$predict)
#length(H2OSecondLayerDeepLearning.prediction$predict)
print(c('Survived Percentage% ',length(True.Values$predict)/length(H2OSecondLayerDeepLearning.prediction$predict)))
print(c('DEAD Percentage% ',length(False.Values$predict)/length(H2OSecondLayerDeepLearning.prediction$predict)))



H2OSecondLayerDeepLearning.prediction$id<-1:dim(H2OSecondLayerDeepLearning.prediction)[1]

prediction.data <- data.frame(merge(testing.data[,c('id','PassengerId')],H2OSecondLayerDeepLearning.prediction[,c('id','predict')],by="id"))


# Save the predictions
submission.data <- data.frame(PassengerID = prediction.data$PassengerId, Survived = prediction.data$predict)

# Write the solution to file
write.csv(submission.data, file = 'Submissions/11ModelAggregateWithPredictorsSecondLayer.csv', row.names = F)
#write.csv(submission.data, file = 'H2oRandomForest_Predictions3.csv', row.names = F)





