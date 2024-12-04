library(caTools) 
library(randomForest)
library(caret)
library(tidyverse)

setwd("/Users/hoover/Documents/GitHub/coPlateauWaterQuality/03_data/")
#setwd("/Users/aaronnuanez/Documents/GitHub/coPlateauWaterQuality/03_data")

rm(list=ls())

#Load data
Asdata <- read.csv("All_As_Data.csv")

classifier_RF<-readRDS("2024-11-25_classLTE1_cv10_rf.rds")

# Filter data into train and test sets based on logical variable 'trainCat2'
#train <- Asdata2[Asdata2$trainCat2 == TRUE, ] #Need up update this field and dataframe to match what is produce in lines 21-24
test <- Asdata[Asdata$trainClassLTE1_splt == FALSE, ] #Need up update this field and dataframe to match what is produce in lines 21-24

#Make SiteID the row name so we can drop that field
rownames(test)<-test$SiteID

#Drop unused fields
AsTest<-test[,-c(1, 4, 109:112, 158:168)] #Drop the As concentration, and the categorical variables we already transformed
#AsTest<-test[,-c(1, 4, 109:112, 157, 159:168)]  #LTE2
#AsTest<-test[,-c(1, 4, 109:112, 157:158, 160:168)]  #LTE3
#AsTest<-test[,-c(1, 4, 109:112, 157:159, 161:168)]  #LTE5
#AsTest<-test[,-c(1, 4, 109:112, 157:160, 162:168)]  #LTE10


#Ensure ClassLTE1 is a Factor (Categorical Variable)
AsTest$ClassLTE1 <- as.factor(AsTest$ClassLTE1)
#AsTest$ClassLTE2  <- as.factor(AsTest$ClassLTE2)
#AsTest$ClassLTE3  <- as.factor(AsTest$ClassLTE3)
#AsTest$ClassLTE5  <- as.factor(AsTest$ClassLTE5)
#AsTest$ClassLTE10  <- as.factor(AsTest$ClassLTE10)


classifier_RF

# Predicting the Test set results 
y_pred <- predict(classifier_RF, newdata = AsTest)

# Confusion Matrix 
confusion_mtx <- confusionMatrix(y_pred, AsTest$ClassLTE1)
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
  mutate(Avgkap = mean(Kappa)) %>%
  mutate(AvgSen = mean(sensitivity)) %>%
  mutate(AvgSpec = mean(specificity))


# Test data values
accuracy
kappa_value
sensitivity
specificity

importance <- varImp(classifier_RF, scale = TRUE)

# Plot variable importance
plot(importance, top = 10, col = "blue",  main = "Random Forest Classification")

