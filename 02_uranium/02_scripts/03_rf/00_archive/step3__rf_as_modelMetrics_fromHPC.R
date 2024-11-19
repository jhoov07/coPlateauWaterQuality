library(caTools) 
library(randomForest)
library(caret)
library(tidyverse)

#Set working directory
setwd("/Users/hoover/Documents/GitHub/coPlateauWaterQuality/02_uranium")
#setwd("/Users/austinmartinez/Documents/GitHub/coPlateauWaterQuality/01_data")

#Clean up the workspace
rm(list=ls())

#Load RF model
classifier_RF<-readRDS("./03_modelOuput/01_rf/2024-07-30_mtry5_cv10_bU30_final_model_rf.rds")

#Load data
data = read.csv("./01_data/Cleaned_U_GIS_Filtered_Joe.csv",
                na.strings = c("NULL", "NA")) #Probably need to simplify the path so the script and data are in the same folder for the HPC

data <- data[complete.cases(data), ]

#Subset to test set
test<-subset(data, trainCat3==FALSE)

# Predicting the Test set results 
y_pred = predict(classifier_RF, newdata = test[,-c(1:5,208:215, 217:218)]) 

# Predicting the probability of each class for Test set results 
y_pred = predict(classifier_RF, newdata = AsTest[,-c(1:6,213:215, 217)], type="prob") 

# Confusion Matrix 
#confusion_mtx = table(AsTest[,216], y_pred) 
#confusion_mtx

confusionMatrix(y_pred, factor(test[,216]))

# Plotting model 
plot(classifier_RF)

#Calculate variable importance
gbmImp <- varImp(classifier_RF, scale = FALSE)
gbmImp

#Plot variable importance
# - Variable importance
plot(gbmImp, top=10) 
