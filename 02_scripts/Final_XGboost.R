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
AsTrain<-train[,-c(1:5,212:214, 216:217)]
AsTest<-test[,-c(1:5,212:214, 216:217)]

#Ensure As3Cat is a Factor (Categorical Variable)
AsTrain$As3Cat <- as.factor(AsTrain$As3Cat)
AsTest$As3Cat <- as.factor(AsTest$As3Cat)

AsTrain<-AsTrain[,-208]
AsTest<-AsTest[,-208]

#define predictor and response variables in training set
train_x<-data.matrix(AsTrain)
train_y<-AsTrain[,207]

#define predictor and response variables in testing set
test_x<-data.matrix(AsTest)
test_y<-AsTest[,207]

model =  readRDS("/Users/austinmartinez/Documents/GitHub/coPlateauWaterQuality/03_modelOutputs/03_xgb/2024-07-26_As3Cat_cv5_xgb.rds")

#This model took ~5 minutes to run on my laptop 
model<-train(
  factor(As3Cat) ~ ., 
  data = AsTrain, 
  metric = "Accuracy",
  method = "xgbTree",
  trControl = trainControl(method="cv", number = 10),
  tuneGrid = expand.grid(
    nrounds = 500,
    max_depth = c(10),
    eta = c(0.01),  #Shrinkage
    gamma = 0,
    colsample_bytree = c(0.75),
    min_child_weight = 1,
    subsample = c(0.5)
  )
)


model

# training data accuracy and kappa
# AvgAcc = accuracy  Avgkap = kappa
model$resample %>%
  arrange(Resample) %>%
  mutate (AvgAcc = mean(Accuracy)) %>%
  mutate (Avgkap = mean(Kappa))

#make prediction and take out collum 207 from data set
predictions <- predict(model, newdata = AsTest[-207])

#calculates the confusion matrix
conf_matrix <- confusionMatrix(predictions, factor(test_y))
conf_matrix

#extract sensitivity, specificity, and balanced accuracy
sensitivity <- conf_matrix$byClass[, "Sensitivity"]
specificity <- conf_matrix$byClass[, "Specificity"]
accuracy <- conf_matrix$overall['Accuracy']
kappa_value <- conf_matrix$overall['Kappa']

# Test data values
accuracy
kappa_value
sensitivity
specificity

importance <- varImp(model, scale = FALSE)

# Plot variable importance
plot(importance, top = 10, col = "blue",  main = "XGBoost")
