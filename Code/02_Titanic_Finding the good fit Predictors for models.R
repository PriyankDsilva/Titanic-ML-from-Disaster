#Set Working Environment
setwd('C:/Users/Priyank/OneDrive/Documents/MIS/Predictive Analysis/Titanic')

#Load data
load('RDFiles/TitanicManualImputation.Rd')

#required Libraries
library(h2o)
library(randomForest)
library(plyr)
library(kernlab)
library(gbm)
library(nnet)
library(RSofia)
library(deepnet)
library(dplyr)  
#install.packages('deepnet')

summary(training.data)

#colnames(training.data)

#Seperate Data
#testing.data <- training.data[892:1309,]
testing.data <- training.data[701:891,]
#testing.data <- training.data[1:891,]

#training.data <-training.data[1:891,]
training.data <-training.data[1:700,]
#training.data <-training.data[1:891,]

ModelScore<- c('False','True','Total','%','Predictors')

# Start h2o ,decide min and max memory sizes,number of cpu cores to be used (-1 means all cores)
localH2O <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE,
                     max_mem_size = '12g', min_mem_size = '4g', nthreads = -1)



#Training data
training.data.hex<-as.h2o(localH2O, training.data)


predictors <-  c('Pclass','Sex','SibSp','Parch','Embarked','GenaralizedAge','FamilySize','FamilySizeCategory',
                 'Child','Parent','FareCategory','Title','FamilySizeCategory','FamilyFlag',
                  'Mother','RoundedFillAge','RoundedFare')

#computation purmurtation
for(n in seq(from=2,to=length(predictors))){
  print(c('Iteration : ',n))
  combination <- combn(unique(predictors),n)
  
  for(m in seq(from=1,to=dim(combination)[2])){
    predict_vars <-(combination[,m])
    print(predict_vars)
    #Combined.Predictors <- rbind(Combined.Predictors,combination[,m])

  
#Code to having final prediction
#for(x in seq(from=1,to=length(Combined.Predictors))){
#predict_vars <- unlist(strsplit(Combined.Predictors[x], split=","))



#######################################H2O Models#######################################################
#use maximum size
memory.size(TRUE)
gc()

#crete H2O model
H2ODeepLearning <- h2o.deeplearning(x = predict_vars, y = 'Survived' ,  training_frame = training.data.hex,
                                    model_id='H2ODeepLearning', epochs = 500)

H2OGBM <- h2o.gbm(x = predict_vars, y = 'Survived' ,  training_frame = training.data.hex,
                  model_id='H2OGBM',ntrees = 500)

H2OGLM <- h2o.glm(x = predict_vars, y = 'Survived' ,  training_frame = training.data.hex,model_id='H2OGLM')

H2ONaiveBayes <- h2o.naiveBayes(x = predict_vars, y = 'Survived' ,  training_frame = training.data.hex,model_id='H2ONaiveBayes')

H2ORandomForest <- h2o.randomForest(x = predict_vars, y = 'Survived' ,  training_frame = training.data.hex,
                                    model_id='H2ORandomForest',ntrees = 1000)

#?h2o.deeplearning
#?h2o.gbm
#?h2o.glm
#?h2o.naiveBayes
#?h2o.randomForest


#predict values for the Testing dataset 
testing.data.hex<-as.h2o(localH2O, testing.data[,predict_vars])


#H2ODeepLearning
H2ODeepLearning.prediction.hex <- h2o.predict(H2ODeepLearning, testing.data.hex)
H2ODeepLearning.prediction <- as.data.frame(H2ODeepLearning.prediction.hex)

#H2OGBM
H2OGBM.prediction.hex <- h2o.predict(H2OGBM, testing.data.hex)
H2OGBM.prediction <- as.data.frame(H2OGBM.prediction.hex)

#H2OGLM
H2OGLM.prediction.hex <- h2o.predict(H2OGLM, testing.data.hex)
H2OGLM.prediction <- as.data.frame(H2OGLM.prediction.hex)


#H2ONaiveBayes
H2ONaiveBayes.prediction.hex <- h2o.predict(H2ONaiveBayes, testing.data.hex)
H2ONaiveBayes.prediction <- as.data.frame(H2ONaiveBayes.prediction.hex)


#H2ORandomForest
H2ORandomForest.prediction.hex <- h2o.predict(H2ORandomForest, testing.data.hex)
H2ORandomForest.prediction <- as.data.frame(H2ORandomForest.prediction.hex)



#Merge the Predictions
H2ODeepLearning.prediction$id<-1:dim(H2ODeepLearning.prediction)[1]
H2OGBM.prediction$id<-1:dim(H2OGBM.prediction)[1]
H2OGLM.prediction$id<-1:dim(H2OGLM.prediction)[1]
H2ONaiveBayes.prediction$id<-1:dim(H2ONaiveBayes.prediction)[1]
H2ORandomForest.prediction$id<-1:dim(H2ORandomForest.prediction)[1]
testing.data$id<-1:dim(testing.data)[1]

#prediction <- testing.data[,c('id','PassengerId','Survived')]
prediction <- testing.data[,c('id','PassengerId','Survived',predict_vars)]


#prediction <- H2ODeepLearning.prediction
prediction <- data.frame(merge(prediction,H2ODeepLearning.prediction,by="id"))
names(prediction)[names(prediction)=="predict"] <- "H2ODeepLearning.prediction"

prediction <- data.frame(merge(prediction,H2OGBM.prediction,by="id"))
names(prediction)[names(prediction)=="predict"] <- "H2OGBM.prediction"

prediction <- data.frame(merge(prediction,H2OGLM.prediction,by="id"))
names(prediction)[names(prediction)=="predict"] <- "H2OGLM.prediction"

prediction <- data.frame(merge(prediction,H2ONaiveBayes.prediction,by="id"))
names(prediction)[names(prediction)=="predict"] <- "H2ONaiveBayes.prediction"

prediction <- data.frame(merge(prediction,H2ORandomForest.prediction,by="id"))
names(prediction)[names(prediction)=="predict"] <- "H2ORandomForest.prediction"

prediction <- prediction[,c('id','PassengerId','Survived','H2ODeepLearning.prediction','H2OGBM.prediction',
                            'H2ONaiveBayes.prediction','H2ORandomForest.prediction','H2OGLM.prediction',predict_vars)]

#Round prediction for GLM Model
prediction$RoundedH2OGLM.prediction <- round(prediction$H2OGLM.prediction)

#######################################H2O Models#######################################################


#Creating Formula
f <- as.formula(paste(paste('Survived', collapse = " + "),'~', paste(predict_vars, collapse = " + ")))


#######################################RandomForest#######################################################

RandomForestModel <- randomForest(f,data =training.data, ntree = 1000, importance = TRUE)

summary(RandomForestModel)
importance(RandomForestModel)

RandomForest.prediction <- predict(RandomForestModel, testing.data[,predict_vars])
RandomForest.prediction <- as.data.frame(RandomForest.prediction)
RandomForest.prediction$id<-1:dim(RandomForest.prediction)[1]

#Addto main prediction
prediction <- data.frame(merge(prediction,RandomForest.prediction,by="id"))
names(prediction)[names(prediction)=="RandomForestPrediction"] <- "RandomForest.prediction"


#######################################RandomForest#######################################################


#######################################Support vector Machine#######################################################

SVMModel <- ksvm(f, data = training.data)
SVMModel.prediction <- predict(SVMModel, testing.data[,predict_vars], type = "response")
SVMModel.prediction <- as.data.frame(SVMModel.prediction)
SVMModel.prediction$id<-1:dim(SVMModel.prediction)[1]

#Addto main prediction
prediction <- data.frame(merge(prediction,SVMModel.prediction,by="id"))

#######################################Support vector Machine#######################################################


#######################################GBM#######################################################
GBM.Model <- gbm(f,data = training.data,distribution = "adaboost", n.trees = 1000)

GBM.prediction <- predict(GBM.Model, testing.data[,predict_vars],n.trees = 500, type = "link")
GBM.prediction <- as.data.frame(GBM.prediction)
GBM.prediction <- floor(GBM.prediction)
GBM.prediction$id<-1:dim(GBM.prediction)[1]

#Addto main prediction
prediction <- data.frame(merge(prediction,GBM.prediction,by="id"))


#######################################GBM#######################################################

#######################################GLM#######################################################
GLM.Model <- glm(f,data = training.data,family = binomial(link = "logit"))

GLM.prediction <- predict(GLM.Model, testing.data[,predict_vars], type = "response")
GLM.prediction <- as.data.frame(GLM.prediction)
GLM.prediction <- round(GLM.prediction)
GLM.prediction$id<-1:dim(GLM.prediction)[1]

#Addto main prediction
prediction <- data.frame(merge(prediction,GLM.prediction,by="id"))


#######################################GLM#######################################################

#######################################NeuralNet#######################################################

NeuralNetModel <- nnet(f,data = training.data, size = 3,maxit=1000)

NeuralNet.prediction <- predict(NeuralNetModel, testing.data[,predict_vars], type = "class")
NeuralNet.prediction <- as.data.frame(NeuralNet.prediction)
NeuralNet.prediction$id<-1:dim(NeuralNet.prediction)[1]

#Addto main prediction
prediction <- data.frame(merge(prediction,NeuralNet.prediction,by="id"))

#######################################NeuralNet#######################################################



#######################################sofia#######################################################

#represent survived in -1 and 1

#training.data$NewSurvived <- as.numeric(training.data$Survived)
#training.data$NewSurvived[training.data$NewSurvived == 1] <- -1
#training.data$NewSurvived[training.data$NewSurvived == 2] <- 1

#testing.data$NewSurvived <- 0

#New Formula
#Sofiaf <- as.formula(paste(paste('NewSurvived', collapse = " + "),'~', paste(predict_vars, collapse = " + ")))


#SofiaModel <- sofia(Sofiaf,data = training.data,learner_type="logreg-pegasos")
#SofiaModel <- sofia(Sofiaf,data = training.data)

#Sofia.prediction <- predict(SofiaModel, testing.data[,c(predict_vars,'NewSurvived')], prediction_type = "logistic")
#Sofia.prediction <- as.data.frame(Sofia.prediction)
#Sofia.prediction <- round(Sofia.prediction)
#Sofia.prediction$id<-1:dim(Sofia.prediction)[1]


#training.data$NewSurvived <- NULL
#testing.data$NewSurvived <- NULL


#Addto main prediction
#prediction <- data.frame(merge(prediction,Sofia.prediction,by="id"))
prediction$Sofia.prediction <- 0

#######################################sofia#######################################################


#store in proper order
#Format prediction 
prediction <- prediction[,c(1:8,(length(predict_vars)+9):dim(prediction)[2],9:(length(predict_vars)+8))]

prediction$H2OGLM.prediction <- prediction$RoundedH2OGLM.prediction
prediction$RoundedH2OGLM.prediction <- NULL  



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
  prediction$TotalCount[i] <- (as.numeric(prediction$H2ODeepLearning.prediction[i])  
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

#Match
prediction$Matched <- FALSE
#Code to having final prediction
for(i in seq(from=1,to=length(prediction$id))){
  if(prediction$Survived[i] == prediction$FinalPrediction[i] ){
    prediction$Matched[i] <- TRUE
  }
}

prediction.compare <- prediction[,c('Survived','FinalPrediction','TotalCount','Matched',factor_vars)]


summary(prediction.compare$Matched)
summary(as.factor(prediction.compare$FinalPrediction))


True.Values <- filter(prediction.compare, Matched == TRUE )
False.Values <-  filter(prediction.compare, Matched == FALSE )
length(True.Values$Matched)
length(False.Values$Matched)
length(prediction.compare$Matched)
Percentage <- length(True.Values$Matched)/length(prediction.compare$Matched)


ModelScore <- rbind(ModelScore,cbind(length(False.Values$Matched),length(True.Values$Matched),length(prediction.compare$Matched),Percentage,paste0(predict_vars,sep=",",collapse="")))

rm(list=setdiff(ls(), c("x",'Combined.Predictors','ModelScore','prediction','testing.data','training.data',
                        'predictors','m','n','combination','localH2O','training.data.hex','testing.data.hex')))
  }
  ModelScoreName=paste0('RDFiles/ModelScoreAnalysisIteration',n,'.Rd')
  save(ModelScore,file=ModelScoreName)
}

#save(ModelScore,file='RDFiles/ModelScoreInteruptedat5.Rd')
