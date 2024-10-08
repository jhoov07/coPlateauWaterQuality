library(caTools) 
library(randomForest)
library(caret)
library(tidyverse)


#setwd("/Users/hoover/Documents/GitHub/coPlateauWaterQuality/01_data/CoPlateau_As")
setwd("/Users/austinmartinez/Documents/GitHub/coPlateauWaterQuality/01_data/CoPlateau_As")

#Clean up the workspace
rm(list=ls())

Asdata = read.csv("Cleaned_As_GIS_Filtered.csv", na.strings = "NULL")

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

# Fitting Random Forest to the train dataset 

tunegrid <- expand.grid(mtry = (1:2)) #Change to 1:84 if testing for real, 1:3 was used for model development

#this is the more acurate model out put 
#it was ran on the super computer
classifier_RF = readRDS("/Users/austinmartinez/Documents/GitHub/coPlateauWaterQuality/03_modelOutputs/01_randomForest/2024-07-25_rf.rds")

# This model runs in legit 2 seconds
classifier_RF<-train(
  factor(As3Cat) ~ ., 
  data = AsTrain, 
  metric = "Accuracy",
  method = "rf",
  trControl = trainControl(method="cv", number = 10),    #change number = 10 if doing for real
  tuneGrid  = tunegrid,
  ntree = 500,
  verboseIter = TRUE  # Enable verbose output for troubleshooting
)

classifier_RF

# Predicting the Test set results 
y_pred <- predict(classifier_RF, newdata = AsTest) 

# Confusion Matrix 
confusion_mtx <- confusionMatrix(y_pred, AsTest$As3Cat)
confusion_mtx

# Plotting model 
plot(classifier_RF)

# Calculate Accuracy
accuracy <- confusion_mtx$overall['Accuracy']
accuracy

# Calculate kappa value
kappa_value <- confusion_mtx$overall['Kappa']
kappa_value

# Extract Sensitivity and Specificity for each class
sensitivity <- confusion_mtx$byClass[,"Sensitivity"]
sensitivity
specificity <- confusion_mtx$byClass[,"Specificity"]
specificity

# training data accuracy and kappa
# AvgAcc = accuracy  Avgkap = kappa
classifier_RF$resample %>%
  arrange(Resample) %>%
  mutate(AvgAcc = mean(Accuracy)) %>%
  mutate(Avgkap = mean(Kappa))


# Test data values
accuracy
kappa_value
sensitivity
specificity

importance <- varImp(classifier_RF, scale = FALSE)

# Plot variable importance
plot(importance, top = 10, col = "blue",  main = "Random Forest Classification")
