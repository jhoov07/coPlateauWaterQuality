library(caTools) 
library(randomForest)
library(caret)
library(tidyverse)


setwd("/Users/hoover/Documents/GitHub/coPlateauWaterQuality/01_data")
#setwd("/Users/austinmartinez/Documents/GitHub/coPlateauWaterQuality/01_data")

#Clean up the workspace
rm(list=ls())

set.seed(1234)  # Setting seed 

#Load data
Asdata = read.csv("AsModelInput.csv")

# Filter data into train and test sets based on logical variable 'spl3cat'
train <- Asdata[Asdata$spl3cat == TRUE, ]
test <- Asdata[Asdata$spl3cat == FALSE, ]

# Split the data into training (70%) and testing (30%) sets
#sample_indices <- sample(1:nrow(Asdata), 0.7 * nrow(Asdata))
#train_data <- Asdata[sample_indices, ]
#test_data <- Asdata[-sample_indices, ]

# Drop rows with NA values in columns 9 to 93 for test data
AsTrain <- train[complete.cases(train[, 9:93]), ]
AsTest <- test[complete.cases(test[, 9:93]), ]

#Ensure As3Cat is a Factor (Categorical Variable)
AsTrain$As3Cat <- as.factor(AsTrain$As3Cat)
AsTest$As3Cat <- as.factor(AsTest$As3Cat)

# Fitting Random Forest to the train dataset 

#classifier_RF = randomForest(x = AsTrain[,-(1:3,5:8)], y = as.factor(AsTrain$As3Cat), ntree = 100) 
#classifier_RF 
tunegrid <- expand.grid(mtry = (1:3)) #Change to 1:84 if testing for real, 1:3 was used for model development

classifier_RF<-train(
  factor(As3Cat) ~ ., 
  data = AsTrain[,-c(1:3,5:8)], 
  metric = "Accuracy",
  method = "rf",
  trControl = trainControl(method="cv", number = 2),    #change number = 10 if doing for real
  tuneGrid  = tunegrid,
  ntree = 500,
  verboseIter = TRUE  # Enable verbose output for troubleshooting
  )

saveRDS(classifier_RF, "./final_model.rds")

# Predicting the Test set results 
y_pred = predict(classifier_RF, newdata = AsTest[-c(1:8)]) 

# Confusion Matrix 
confusion_mtx = table(AsTest[,4], y_pred) 
confusion_mtx

# Plotting model 
plot(classifier_RF) 

# - Variable importance 
var_importance <- varImpPlot(classifier_RF) 
var_importance

# Importance
importance <- importance(classifier_RF)
importance

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

