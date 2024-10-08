library(caTools) 
library(randomForest)
library(caret)
library(tidyverse)

#Set working directory
setwd("/Users/hoover/Documents/GitHub/coPlateauWaterQuality")
#setwd("/Users/austinmartinez/Documents/GitHub/coPlateauWaterQuality/01_data")

#Clean up the workspace
rm(list=ls())

#Load RF model
classifier_RF<-readRDS("./03_modelOutputs/01_randomForest/2024-07-25_rf.rds")

#Load data
Asdata = read.csv("./01_data/CoPlateau_As/20240723_randomForest_As_dataClean.csv",
                  na.strings = "NA") #Probably need to simplify the path so the script and data are in the same folder for the HPC

Asdata <- Asdata[complete.cases(Asdata), ]

#Subset to test set
AsTest<-subset(Asdata, trainCat3==FALSE)

# Predicting the Test set results 
y_pred = predict(classifier_RF, newdata = AsTest[,-c(1:6,213:215, 217)]) 

# Predicting the probability of each class for Test set results 
y_pred = predict(classifier_RF, newdata = AsTest[,-c(1:6,213:215, 217)], type="prob") 

# Confusion Matrix 
#confusion_mtx = table(AsTest[,216], y_pred) 
#confusion_mtx

confusionMatrix(y_pred, factor(AsTest[,216]))

# Plotting model 
plot(classifier_RF) 

#Calculate variable importance
gbmImp <- varImp(classifier_RF, scale = TRUE)
gbmImp

#Plot variable importance
# - Variable importance
plot(gbmImp, top=20) 
