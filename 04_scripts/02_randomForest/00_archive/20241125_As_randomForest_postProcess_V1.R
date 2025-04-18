library(caTools) 
library(randomForest)
library(caret)
library(tidyverse)


setwd("/Users/hoover/Downloads")
#setwd("/Users/aaronnuanez/Documents/GitHub/coPlateauWaterQuality/03_data")


rm(list=ls())

#Load data
Asdata <- read.csv("All_As_Data.csv")

# Filter data into train and test sets based on logical variable 'trainCat2'
train <- Asdata[Asdata$trainClassLTE2_splt == TRUE, ] #Need up update this field and dataframe to match what is produce in lines 21-24
test <- Asdata[Asdata$trainClassLTE2_splt == FALSE, ] #Need up update this field and dataframe to match what is produce in lines 21-24

#Make SiteID the row name so we can drop that field
rownames(train)<-train$SiteID
rownames(test)<-test$SiteID

#Drop unused fields
AsTrain<-train[,-c(1, 4, 109:112, 157, 159:168)] #Drop the As concentration, and the categorical variables we already transformed
AsTest<-test[,-c(1, 4, 109:112, 157, 159:168)]

#Ensure ClassLTE2 is a Factor (Categorical Variable)
AsTrain$ClassLTE2 <- as.factor(AsTrain$ClassLTE2)
AsTest$ClassLTE2  <- as.factor(AsTest$ClassLTE2)


#Load data
classifier_RF <- readRDS("2025-01-31_rf.rds")
classifier_RF

#best mtry is 54

# Predicting the Test set results 
y_pred <- predict(classifier_RF, newdata = AsTest)

# Confusion Matrix 
confusion_mtx <- confusionMatrix(y_pred, AsTest$ClassLTE2)
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
sensitivity <- confusion_mtx$byClass[[1]]
sensitivity
specificity <- confusion_mtx$byClass[[2]]
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

