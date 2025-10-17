
# Installing package 
#install.packages("caTools")       # For sampling the dataset 
#install.packages("randomForest")  # For implementing random forest algorithm 
#install.packages("caret")

# Loading package 
library(caTools) 
library(randomForest)
library(caret)
library(tidyverse)

setwd("C:/Users/jhoover/Documents/GitHub/coPlateauWaterQuality/01_data")


#Clean up space
rm(list=ls())

#Load data
d<-read.csv("AsModelInput.csv")

# Splitting data in train and test data 
#split <- sample.split(d, SplitRatio = 0.7) 
#split 

train <- subset(d, spl3cat == "TRUE") 
test <- subset(d, spl3cat == "FALSE")

#Ensure no NAs because RFC can't handle NAs
dTrain <- train %>%
  drop_na(c(9:93))

dTest <- test %>%
  drop_na(c(9:93))

# Fitting Random Forest to the train dataset 
set.seed(120)  # Setting seed 
classifier_RF = randomForest(x = dTrain[,-(1:8)], 
                             y = as.factor(dTrain$As3Cat), 
                             ntree = 100) 

classifier_RF 

# Predicting the Test set results 
y_pred = predict(classifier_RF, newdata = dTest[-c(1:8)]) 

# Confusion Matrix 
confusion_mtx = table(dTest[,4], y_pred) 
caret::confusion_mtx

# Plotting model 
plot(classifier_RF) 

# Importance plot 
importance(classifier_RF) 

# Variable importance plot 
varImpPlot(classifier_RF) 
