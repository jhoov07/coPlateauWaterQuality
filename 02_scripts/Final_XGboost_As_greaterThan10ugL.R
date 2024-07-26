library(caTools) 
library(caret)
library(gbm)
library(xgboost) # for xgboost
library(tidyverse) # general utility functions
library(smotefamily) #use to balance the training dataset

#setwd("/Users/hoover/Documents/GitHub/coPlateauWaterQuality/01_data")
setwd("/Users/austinmartinez/Documents/GitHub/coPlateauWaterQuality/01_data/CoPlateau_As")

#Clean up the workspace
rm(list=ls())

#Load data
Asdata = read.csv("Cleaned_As_GIS_Filtered.csv", na.strings = "NULL")

# set a random seed & shuffle data frame
set.seed(1234)

#take out NAs
Asdata <- Asdata[complete.cases(Asdata), ]


# Filter data into train and test sets based on logical variable 'spl3cat'
train <- Asdata[Asdata$spl3cat == TRUE, ]
test <- Asdata[Asdata$spl3cat == FALSE, ]


#Drop unused fields
AsTrain<-train[,-c(1:5,212:213, 215:217)]
AsTest<-test[,-c(1:5,212:213, 215:217)]

#Ensure As3Cat is a Factor (Categorical Variable)
AsTrain$As3Cat <- as.factor(AsTrain$bas10)
AsTest$As3Cat <- as.factor(AsTest$bas10)

AsTrain<-AsTrain[,-208]
AsTest<-AsTest[,-208]

#define predictor and response variables in training set
train_x<-data.matrix(AsTrain)
train_y<-AsTrain[,207]

#define predictor and response variables in testing set
test_x<-data.matrix(AsTest)
test_y<-AsTest[,207]

model = readRDS("/Users/austinmartinez/Documents/GitHub/coPlateauWaterQuality/03_modelOutputs/03_xgb/2024-07-24_xgb_10ugL.rds")

#This model took ~5 minutes to run on my laptop 
model<-train(
  factor(bas10) ~ ., 
  data = AsTrain, 
  metric = "Accuracy",
  method = "xgbTree",
  trControl = trainControl(method="cv", number = 3),
  tuneGrid = expand.grid(
    nrounds = 20,
    max_depth = c(6, 8, 10),
    eta = c(0.01, 0.02),  #Shrinkage
    gamma = 0,
    colsample_bytree = c(0.5, 0.75),
    min_child_weight = 1,
    subsample = c(0.5, 0.75)
  )
)


model

model$resample %>%
  arrange(Resample)

#make prediction and take out bas1 from data set
predictions <- predict(model, newdata = AsTest)

#calculates the confusion matrix
conf_matrix <- confusionMatrix(predictions, factor(test_y))

#extract sensitivity, specificity, and balanced accuracy
sensitivity <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
balanced_accuracy <- conf_matrix$byClass["Balanced Accuracy"]

#results
sensitivity
specificity
balanced_accuracy
conf_matrix

importance <- varImp(model, scale = FALSE)

# Plot variable importance
plot(importance, top = 10, col = "blue",  main = "XGBoost")
