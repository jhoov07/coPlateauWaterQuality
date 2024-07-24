library(caTools) 
library(randomForest)
library(caret)
library(tidyverse)

#Set working directory
setwd("/Users/hoover/Documents/GitHub/coPlateauWaterQuality/01_data")
#setwd("/Users/austinmartinez/Documents/GitHub/coPlateauWaterQuality/01_data")

#Clean up the workspace
rm(list=ls())

date<-Sys.Date()
set.seed(1234)  # Setting seed 

#Load data
Asdata = read.csv("./CoPlateau_As/20240723_randomForest_As_dataClean.csv") #Probably need to simplify the path so the script and data are in the same folder for the HPC

#Subset to training set
As_train<-subset(Asdata, trainCat3==TRUE)


#Ensure As3Cat is a Factor (Categorical Variable)
As_train$As3Cat <- as.factor(As_train$As3Cat)

#Drop unused fields
AsTrain<-As_train[,-c(1:6,213:215, 217)]

# Fitting Random Forest to the train dataset 

#classifier_RF 
n<-ncol(AsTrain)-1
tunegrid <- expand.grid(mtry = n) #Change to 1:84 if testing for real, 1:3 was used for model development

classifier_RF<-train(
  factor(As3Cat) ~ ., 
  data = AsTrain[,-1], 
  metric = "Accuracy",
  method = "rf",
  trControl = trainControl(method="cv", number = 1),    #change number = 10 if doing for real
  tuneGrid  = tunegrid,
  ntree = 500,
  verboseIter = TRUE  # Enable verbose output for troubleshooting
  )

saveRDS(classifier_RF, paste("./",date, "_final_model_rf.rds", sep=""))
