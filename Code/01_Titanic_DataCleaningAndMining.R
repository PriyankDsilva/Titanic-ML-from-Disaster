#Set Working Environment
setwd('C:/Users/Priyank/OneDrive/Documents/MIS/Predictive Analysis/Titanic')

# Load packages
library('ggplot2') # visualization
library('ggthemes') # visualization
library('mice') # imputation
library('randomForest') # classification algorithm
library('dplyr') # data manipulation


#Read Data for Training

training.data <- read.csv('Source/train.csv', stringsAsFactors = F,na.strings = c("", " "))
testing.data <- read.csv('Source/test.csv', stringsAsFactors = F,na.strings = c("", " "))

training.data  <- bind_rows(training.data, testing.data)



###################################################################################################################
#Data Mining and Cleansing
###################################################################################################################

#Passenger Name --> Name Title

training.data$Title <- gsub('(.*, )|(\\..*)', '', training.data$Name)

#View Distribution of values
table(training.data$Sex, training.data$Title)

Valid_titles <- c('Miss','Mrs','Master','Mr')

training.data$Title[training.data$Title == 'Mlle'] <- 'Miss' #Mademoiselle in French
training.data$Title[training.data$Title == 'Ms'] <- 'Miss'
training.data$Title[training.data$Title == 'Mme'] <- 'Mrs' #Madame in French

training.data$Title[!(training.data$Title %in% Valid_titles)]  <- 'Others'


#Family Surname
training.data$Surname <- sapply(training.data$Name,function(x) strsplit(x, split = '[,.]')[[1]][1])
training.data$FamilySize <- training.data$SibSp + training.data$Parch + 1 

training.data$Family <- paste(training.data$Surname, training.data$FamilySize, sep='_')

#Family Flag
training.data$FamilyFlag <- 1

#identify family and alone passengers
for(i in seq(from=1,to=length(training.data$Family))){
  if(training.data$FamilySize[i] == 1){
    training.data$FamilyFlag[i] <- 0
  }
}




#Plot to see the graph for survival based on family size
ggplot(training.data, aes(x = FamilySize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()

#categorize family based on graph
#1- singleton
#2-4 - small
#5 and above - large

training.data$FamilySizeCategory[training.data$FamilySize == 1] <- 'singleton'
training.data$FamilySizeCategory[training.data$FamilySize < 5 & training.data$FamilySize > 1] <- 'small'
training.data$FamilySizeCategory[training.data$FamilySize > 4] <- 'large'

# Show family size by survival using a mosaic plot
mosaicplot(table(training.data$FamilySizeCategory, training.data$Survived), main='Family Size by Survival', shade=TRUE)


###################################################################################################################
#New variable Deck based on the Cabin

training.data$Deck<-factor(sapply(training.data$Cabin, function(x) strsplit(x, NULL)[[1]][1]))

#Set 0 Fare to NA
training.data$Fare[training.data$Fare == 0] <- NA


###################################################################################################################
#Missing data'
#Age-Title,FamilySize,Sibsp,Parch,Sex,Fare
#Embark-Fare,Pclass
#Deck(Derived)-Tickets,Fare,Pclass
#Fare


# Embarked
training.data[c(62, 830),]

#Find the Occurrence
as.data.frame(table(training.data$Embarked))

# only in titanic_df, fill the two missing values with the most occurred value, which is "S".
training.data$Embarked[is.na(training.data$Embarked)] <- 'S'


#Age

#Master
training.data.Master<- training.data[training.data$Title=='Master', ]
Master.Average <- mean(training.data.Master$Age,na.rm=TRUE)
Master.SD <- sd(training.data.Master$Age,na.rm=TRUE)

#Code for random numbers
#sample((Master.Average-Master.SD):(Master.Average+Master.SD), 1)

#Code to fill na values with randon mnumbers
for(i in seq(from=1,to=length(training.data.Master$Age))){
  if(is.na(training.data.Master$Age[i]) == TRUE){
    training.data.Master$Age[i] <- sample((Master.Average-Master.SD):(Master.Average+Master.SD), 1)
  }
}

#Miss
training.data.Miss<- training.data[training.data$Title=='Miss', ]
Miss.Average <- mean(training.data.Miss$Age,na.rm=TRUE)
Miss.SD <- sd(training.data.Miss$Age,na.rm=TRUE)


#Code to fill na values with randon mnumbers
for(i in seq(from=1,to=length(training.data.Miss$Age))){
  if(is.na(training.data.Miss$Age[i]) == TRUE){
    training.data.Miss$Age[i] <- sample((Miss.Average-Miss.SD):(Miss.Average+Miss.SD), 1)
  }
}

#Mr
training.data.Mr<- training.data[training.data$Title=='Mr', ]
Mr.Average <- mean(training.data.Mr$Age,na.rm=TRUE)
Mr.SD <- sd(training.data.Mr$Age,na.rm=TRUE)


#Code to fill na values with randon mnumbers
for(i in seq(from=1,to=length(training.data.Mr$Age))){
  if(is.na(training.data.Mr$Age[i]) == TRUE){
    training.data.Mr$Age[i] <- sample((Mr.Average-Mr.SD):(Mr.Average+Mr.SD), 1)
  }
}

#Mrs
training.data.Mrs<- training.data[training.data$Title=='Mrs', ]
Mrs.Average <- mean(training.data.Mrs$Age,na.rm=TRUE)
Mrs.SD <- sd(training.data.Mrs$Age,na.rm=TRUE)


#Code to fill na values with randon mnumbers
for(i in seq(from=1,to=length(training.data.Mrs$Age))){
  if(is.na(training.data.Mrs$Age[i]) == TRUE){
    training.data.Mrs$Age[i] <- sample((Mrs.Average-Mrs.SD):(Mrs.Average+Mrs.SD), 1)
  }
}

#Others
training.data.Others<- training.data[training.data$Title=='Others', ]
Others.Average <- mean(training.data.Others$Age,na.rm=TRUE)
Others.SD <- sd(training.data.Others$Age,na.rm=TRUE)


#Code to fill na values with randon mnumbers
for(i in seq(from=1,to=length(training.data.Others$Age))){
  if(is.na(training.data.Others$Age[i]) == TRUE){
    training.data.Others$Age[i] <- sample((Others.Average-Others.SD):(Others.Average+Others.SD), 1)
  }
}


#group all the filled Ages
training.data.NewAge <- rbind(training.data.Master,training.data.Miss,training.data.Mr,training.data.Mrs,training.data.Others)
training.data.NewAge <- training.data.NewAge[,c('PassengerId','Age')]
#Rename
names(training.data.NewAge)[names(training.data.NewAge)=="Age"] <- "FillAge"


# Plot age distributions
par(mfrow=c(1,2))
hist(training.data$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(training.data.NewAge$FillAge, freq=F, main='Age: Imputed Output', 
     col='lightgreen', ylim=c(0,0.04))




#Merging Data fromes
training.data <- data.frame(merge(training.data,training.data.NewAge,by="PassengerId"))

#Round Age
training.data$RoundedFillAge <- round(training.data[,'FillAge'])
training.data$RoundedFillAge[training.data$RoundedFillAge == 0] <- 1

#Sample
Age.Compare <- training.data[,c('PassengerId','Age','FillAge','RoundedFillAge')]




#Fare

#Analyse data for class 1 and Embarked S
#training.data[training.data$Pclass==1 & training.data$Embarked=='S', ]
#NAs only found for family size 1

training.data.Fare1S1<- training.data[training.data$Pclass==1 & training.data$Embarked=='S' & training.data$FamilySize == 1, ]
Fare1S1.Average <-mean(training.data.Fare1S1$Fare,na.rm=TRUE) 
Fare1S1.SD <-sd(training.data.Fare1S1$Fare,na.rm=TRUE)

#Code to fill na values with randon mnumbers
for(i in seq(from=1,to=length(training.data.Fare1S1$Fare))){
  if(is.na(training.data.Fare1S1$Fare[i]) == TRUE){
    training.data.Fare1S1$Fare[i] <- sample((Fare1S1.Average-Fare1S1.SD):(Fare1S1.Average+Fare1S1.SD), 1)
  }
}


#Analyse data for class 2 and Embarked S
#training.data[training.data$Pclass==2 & training.data$Embarked=='S', ]
#NAs only found for family size 1

training.data.Fare2S1<- training.data[training.data$Pclass==2 & training.data$Embarked=='S' & training.data$FamilySize == 1, ]
Fare2S1.Average <-mean(training.data.Fare2S1$Fare,na.rm=TRUE) 
Fare2S1.SD <-sd(training.data.Fare2S1$Fare,na.rm=TRUE)

#Code to fill na values with randon mnumbers
for(i in seq(from=1,to=length(training.data.Fare2S1$Fare))){
  if(is.na(training.data.Fare2S1$Fare[i]) == TRUE){
    training.data.Fare2S1$Fare[i] <- sample((Fare2S1.Average-Fare2S1.SD):(Fare2S1.Average+Fare2S1.SD), 1)
  }
}

#Analyse data for class 3 and Embarked S
#training.data[training.data$Pclass==3 & training.data$Embarked=='S', ]
#NAs only found for family size 1

training.data.Fare3S1<- training.data[training.data$Pclass==3 & training.data$Embarked=='S' & training.data$FamilySize == 1, ]
Fare3S1.Average <-mean(training.data.Fare3S1$Fare,na.rm=TRUE) 
Fare3S1.SD <-sd(training.data.Fare3S1$Fare,na.rm=TRUE)

#Code to fill na values with randon mnumbers
for(i in seq(from=1,to=length(training.data.Fare3S1$Fare))){
  if(is.na(training.data.Fare3S1$Fare[i]) == TRUE){
    training.data.Fare3S1$Fare[i] <- sample((Fare3S1.Average-Fare3S1.SD):(Fare3S1.Average+Fare3S1.SD), 1)
  }
}

#group all the filled Fares
training.data.NewFare <- rbind(training.data.Fare1S1,training.data.Fare2S1,training.data.Fare3S1)
training.data.NewFare <- training.data.NewFare[,c('PassengerId','Fare')]
#Rename
names(training.data.NewFare)[names(training.data.NewFare)=="Fare"] <- "FillFare"


#Merging Data fromes
training.data <- data.frame(merge(training.data,training.data.NewFare,by="PassengerId",all = T))

#Sample
Fare.Compare <- training.data[,c('PassengerId','Fare','FillFare')]

#Code to fill na values with filled Fares
for(i in seq(from=1,to=length(training.data$Fare))){
  if(is.na(training.data$Fare[i]) == TRUE){
    training.data$Fare[i] <- training.data$FillFare[i]
  }
}

#Remove FillFare
training.data$FillFare <- NULL

#Rounded Fare 
training.data$RoundedFare <- round(training.data[,'Fare'])

#Individual FAre
training.data$IndividualFare <- training.data$RoundedFare
#Code to individual fare
for(i in seq(from=1,to=length(training.data$IndividualFare))){
  if(training.data$FamilySize[i]>1){
    training.data$IndividualFare[i] <- round(training.data$RoundedFare[i]/training.data$FamilySize[i])
  }
}




###################################################################################################################
#Additional Columns

#Child
# Create the column child, and indicate whether child or adult
training.data$Child[training.data$RoundedFillAge <= 16] <- 'Child'
training.data$Child[training.data$RoundedFillAge > 16] <- 'Adult'

# Show counts
table(training.data$Child, training.data$Survived)


# Adding Parent variable
training.data$Parent <- 'Not Parent'
training.data$Parent[training.data$Parch > 0 & training.data$RoundedFillAge > 16 & (training.data$Title != 'Miss' | training.data$Title != 'Master')] <- 'Parent'

# Show counts
table(training.data$Parent, training.data$Survived)

# Adding Mother variable
training.data$Mother <- 'Not Mother'
training.data$Mother[training.data$Sex == 'female' & training.data$Parch > 0 & training.data$RoundedFillAge > 16 & training.data$Title != 'Miss'] <- 'Mother'

# Show counts
table(training.data$Mother, training.data$Survived)

# Adding Father variable
training.data$Father <- 'Not Father'
training.data$Father[training.data$Sex == 'male' & training.data$Parch > 0 & training.data$RoundedFillAge > 16 & training.data$Title != 'Master'] <- 'Father'

# Show counts
table(training.data$Father, training.data$Survived)


# Adding more Generalized variable - children / Male / Female

training.data$GenaralizedAge <-'Children'
training.data$GenaralizedAge[training.data$Sex == 'male' & training.data$RoundedFillAge > 16 ] <- 'Male'
training.data$GenaralizedAge[training.data$Sex == 'female' & training.data$RoundedFillAge > 16 ] <- 'Female'

# Show counts
table(training.data$GenaralizedAge, training.data$Survived)


#check status on Family
# Show counts
table(training.data$FamilyFlag, training.data$Survived)


#Column for Fare Category
training.data$FareCategory <- NULL

#Code to categorize the fare
for(i in seq(from=1,to=length(training.data$RoundedFare))){
  if(training.data$RoundedFare[i]>=0 & training.data$RoundedFare[i]<26){
    training.data$FareCategory[i] <- '0-25$'
  }
  if(training.data$RoundedFare[i]>25 & training.data$RoundedFare[i]<76){
    training.data$FareCategory[i] <- '26-75$'
  }
  if(training.data$RoundedFare[i]>75){
    training.data$FareCategory[i] <- '75+$'
  }
}

#Column for Fare Category
training.data$FareCategoryIndividual <- NULL

#Code to categorize the fare
for(i in seq(from=1,to=length(training.data$IndividualFare))){
  if(training.data$IndividualFare[i]>=0 & training.data$IndividualFare[i]<26){
    training.data$FareCategoryIndividual[i] <- '0-25$'
  }
  if(training.data$IndividualFare[i]>25 & training.data$IndividualFare[i]<76){
    training.data$FareCategoryIndividual[i] <- '26-75$'
  }
  if(training.data$IndividualFare[i]>75){
    training.data$FareCategoryIndividual[i] <- '75+$'
  }
}



#Count
table(training.data$FareCategory, training.data$Survived)
table(training.data$FareCategoryIndividual, training.data$Survived)


#Sex
training.data$BooleanSex0M1F <- 0

for(i in seq(from=1,to=length(training.data$BooleanSex0M1F))){
  if(training.data$Sex[i] == 'female'){
    training.data$BooleanSex0M1F[i] <- 1
  }
}



######################################## FORMAT ##########################################3
#Proper Order
training.data <- training.data[,c('PassengerId','Survived','Pclass','Name','Title','Surname','Sex','BooleanSex0M1F',
                                  'GenaralizedAge','Age','FillAge','RoundedFillAge',
                                  'SibSp','Parch','FamilyFlag','FamilySize','Family','FamilySizeCategory',
                                  'Ticket','Embarked','Cabin','Deck','Fare','RoundedFare','FareCategory',
                                  'IndividualFare','FareCategoryIndividual',
                                  'Child','Mother','Father','Parent')]

#Factor Variables
factor_vars <- c('Survived','Pclass','Sex','Embarked','Title','FamilySizeCategory','FareCategoryIndividual',
                 'FamilyFlag','GenaralizedAge','Child','Mother','Parent','Father','FareCategory','BooleanSex0M1F')
training.data[factor_vars] <- lapply(training.data[factor_vars], function(x) as.factor(x))

summary(training.data)



###################################################################################################################

#save and load data in R .Initial data from source file
save(training.data,file='RDFiles/TitanicManualImputation.Rd')

load('RDFiles/TitanicManualImputation.Rd')

write.csv(training.data, file="Source/NewTrainingDataManualImputation.csv", quote=F, row.names=F)


