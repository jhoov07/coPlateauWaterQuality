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

tunegrid <- expand.grid(mtry = (1:3)) #Change to 1:84 if testing for real, 1:3 was used for model development

classifier_RF<-train(
  factor(As3Cat) ~ ., 
  data = AsTrain, 
  metric = "Accuracy",
  method = "rf",
  trControl = trainControl(method="cv", number = 2),    #change number = 10 if doing for real
  tuneGrid  = tunegrid,
  ntree = 50,
  verboseIter = TRUE  # Enable verbose output for troubleshooting
)


# Predicting the Test set results 
y_pred = predict(classifier_RF, newdata = AsTest) 

# Confusion Matrix 
confusion_mtx = table(AsTest[,207], y_pred) 
confusion_mtx

# Plotting model 
plot(classifier_RF) 

# Importance
#importance <- importance(classifier_RF)
#importance

# Calculate Accuracy
accuracy <- sum(diag(confusion_mtx)) / sum(confusion_mtx)
accuracy

# Calculate kappa value
kappa_value <- kappa(confusion_mtx)
kappa_value

#print values
classifier_RF
accuracy
kappa_value

importance <- varImp(classifier_RF, scale = FALSE)

# Plot variable importance
plot(importance, top = 20)
