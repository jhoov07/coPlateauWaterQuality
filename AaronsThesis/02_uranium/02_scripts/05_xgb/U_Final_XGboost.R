library(caTools) 
library(caret)
library(gbm)
library(xgboost) # for xgboost
library(tidyverse) # general utility functions
library(smotefamily) #use to balance the training dataset

#setwd("/Users/hoover/Documents/GitHub/coPlateauWaterQuality/01_data/CoPlateau_U")
setwd("/Users/austinmartinez/Documents/GitHub/coPlateauWaterQuality/01_data/CoPlateau_U")

rm(list=ls())

Udata = read.csv("2Cleaned_U_GIS_Filtered.csv", na.strings = "NULL")

#elevation kepted being viewed as catagorical so made it numeric
Udata$Elevation <- as.numeric(Udata$Elevation)

# set a random seed & shuffle data frame
set.seed(1234)

#take out NAs
Udata <- Udata[complete.cases(Udata), ]


# Filter data into train and test sets based on logical variable 'spl3cat'
train <- Udata[Udata$spl3cat == TRUE, ]
test <- Udata[Udata$spl3cat == FALSE, ]


#Drop unused fields
UTrain<-train[,-c(1:5,208:212)]
UTest<-test[,-c(1:5,208:212)]

#Ensure As3Cat is a Factor (Categorical Variable)
UTrain$U3Cat <- as.factor(UTrain$U3Cat)
UTest$U3Cat <- as.factor(UTest$U3Cat)

#define predictor and response variables in training set
train_x<-data.matrix(UTrain)
train_y<-UTrain[,203]

#define predictor and response variables in testing set
test_x<-data.matrix(UTest)
test_y<-UTest[,203]

#model =  readRDS("/Users/austinmartinez/Documents/GitHub/coPlateauWaterQuality/03_modelOutputs/03_xgb/2024-07-26_As3Cat_cv5_xgb.rds")

#This model took ~5 minutes to run on my laptop 
model<-train(
  factor(U3Cat) ~ ., 
  data = UTrain, 
  metric = "Accuracy",
  method = "xgbTree",
  trControl = trainControl(method="cv", number = 2),
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
predictions <- predict(model, newdata = UTest[-203])

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
table(Udata$U3Cat)
importance <- varImp(model, scale = FALSE)

# Plot variable importance
plot(importance, top = 10, col = "blue",  main = "XGBoost")

