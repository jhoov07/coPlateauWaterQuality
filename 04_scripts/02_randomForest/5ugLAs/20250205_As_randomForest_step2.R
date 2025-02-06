library(caTools) 
library(randomForest)
library(caret)
library(tidyverse)


#setwd("/Users/hoover/Downloads")
setwd("/Users/aaronnuanez/Desktop")


rm(list=ls())

#Load data
Asdata <- read.csv("All_As_Data.csv")

# Filter data into train and test sets based on logical variable 'trainCat2'
train <- Asdata[Asdata$trainClassLTE5_splt == TRUE, ] #Need up update this field and dataframe to match what is produce in lines 21-24
test <- Asdata[Asdata$trainClassLTE5_splt == FALSE, ] #Need up update this field and dataframe to match what is produce in lines 21-24

#Make SiteID the row name so we can drop that field
rownames(train)<-train$SiteID
rownames(test)<-test$SiteID

#Drop unused fields
AsTrain<-train[,-c(1, 4, 109:112, 157:159, 161:168)] #Drop the As concentration, and the categorical variables we already transformed
AsTest<-test[,-c(1, 4, 109:112, 157:159, 161:168)]

#Ensure ClassLTE5 is a Factor (Categorical Variable)
AsTrain$ClassLTE5 <- as.factor(AsTrain$ClassLTE5)
AsTest$ClassLTE5  <- as.factor(AsTest$ClassLTE5)


#Load data
classifier_RF <- readRDS("2025-02-06_rf_5ugL.rds")
classifier_RF

#best mtry is 54

# Predicting the Test set results 
y_pred <- predict(classifier_RF, newdata = AsTest)

# Confusion Matrix 
confusion_mtx <- confusionMatrix(y_pred, AsTest$ClassLTE5)
confusion_mtx

#This output gives us the best model using all variables. Next step is to apply variable selection and use fewer predictors

