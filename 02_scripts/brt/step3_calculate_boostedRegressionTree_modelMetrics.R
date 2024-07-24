library(caTools) 
library(randomForest)
library(caret)
library(tidyverse)
library(gbm)

#Set working directory
setwd("/Users/hoover/Documents/GitHub/coPlateauWaterQuality/01_data")
#setwd("/Users/austinmartinez/Documents/GitHub/coPlateauWaterQuality/01_data")

#Clean up the workspace
rm(list=ls())

#Load RF model
Arsenic_boost<-readRDS("2024-07-23cv3_final_model_brt.rds")

#Load data
Asdata = read.csv("./CoPlateau_As/20240723_randomForest_As_dataClean.csv",
                  na.strings = "NA") #Probably need to simplify the path so the script and data are in the same folder for the HPC

Asdata <- Asdata[complete.cases(Asdata), ]

#Subset to test set
AsTest<-subset(Asdata, trainCat3==FALSE)

# Predicting the Test set results 
y_pred = predict(Arsenic_boost, newdata = AsTest[,-c(1:6,213:215, 217)]) 

# Confusion Matrix 
#confusion_mtx = table(AsTest[,216], y_pred) 
#confusion_mtx

confusionMatrix(y_pred, factor(AsTest[,216]))

# Plotting model 
plot(Arsenic_boost) 

#Calculate variable importance
gbmImp <- varImp(Arsenic_boost, scale=T)
gbmImp

#Plot variable importance
# - Variable importance
plot(gbmImp, top=20) 