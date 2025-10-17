library(caTools) 
library(randomForest)
library(caret)
library(tidyverse)


setwd("/Users/hoover/Documents/GitHub/coPlateauWaterQuality/03_data/")
#setwd("/Users/aaronnuanez/Documents/GitHub/coPlateauWaterQuality/03_data")

rm(list=ls())


# set data and seed values
date<-Sys.Date()
set.seed(1234)  # Setting seed 

#Load data
Asdata <- read.csv("All_As_Data.csv")

# Filter data into train and test sets based on logical variable 'trainCat2'
train <- Asdata[Asdata$trainClassLTE10_splt == TRUE, ] #Need up update this field and dataframe to match what is produce in lines 21-24
test <- Asdata[Asdata$trainClassLTE10_splt == FALSE, ] #Need up update this field and dataframe to match what is produce in lines 21-24

#Make SiteID the row name so we can drop that field
rownames(train)<-train$SiteID
rownames(test)<-test$SiteID

#Drop unused fields
AsTrain<-train[,-c(1, 4, 109:112, 157:160, 162:168)] #Drop the As concentration, and the categorical variables we already transformed
AsTest<-test[,-c(1, 4, 109:112, 157:160, 162:168)]

#Ensure ClassLTE1 is a Factor (Categorical Variable)
AsTrain$ClassLTE10 <- as.factor(AsTrain$ClassLTE10)
AsTest$ClassLTE10  <- as.factor(AsTest$ClassLTE10)

#Run random forest model
#mtry is from step 1, might want to try different number of trees too
model<-randomForest(data=AsTrain, ClassLTE10~., mtry=54, ntree=500, importance = TRUE); 
print(model)

#
varImpPlot(model, sort=T, n.var= 30, main= "Variable Importance", pch=16)
impt<-data.frame(model$importance)
print(impt[order(impt$MeanDecreaseGini, decreasing = TRUE), ]   )

##Seems like a drop around variable #10
# pH, Fe, A_Aragon, C_Cr, DepthToGW, C_Ni, prism30yr, A_Calcite, A_Tot_14A, C_Mo, Top5_S, C_Hematite, A_Cs, Top5_Ni


#Partial dependence plots to help us sort out the impact on an individual variable on As concentrations
partialPlot(model, AsTest, pH, "1", xlab="pH", ylab="As Class", lwd=4, col="green")
partialPlot(model, AsTest, Fe, "1", xlab="Fe", ylab="As Class", lwd=4, col="green")
partialPlot(model, AsTest, DepthToGW, "1", xlab="Depth to Groundwater", ylab="As Class", lwd=4, col="green")
partialPlot(model, AsTest, prism30yr, "1", xlab="Precip", ylab="As Class", lwd=4, col="green")

# Fitting Random Forest to the train dataset 
tunegrid <- expand.grid(mtry = (1:10)) #Change to 1:84 if testing for real, 1:3 was used for model development

#this is the more accurate model out put 
#it was ran on the super computer
#classifier_RF = readRDS("/Users/austinmartinez/Documents/GitHub/coPlateauWaterQuality/03_modelOutputs/01_randomForest/2024-07-25_rf.rds")

# This model runs in legit 2 seconds
classifier_RF<-train(
  data = AsTrain,
  factor(ClassLTE10) ~ ., 
  metric = "Accuracy",
  method = "rf",
  trControl = trainControl(method="cv", number = 5),    #change number = 10 if doing for real
  tuneGrid  = tunegrid,
  ntree = 500,
  verboseIter = TRUE  # Enable verbose output for troubleshooting
)

classifier_RF

# Predicting the Test set results 
y_pred <- predict(classifier_RF, newdata = AsTest)

# Confusion Matrix 
confusion_mtx <- confusionMatrix(y_pred, AsTest$ClassLTE10)
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



# Plot variable importance
plot(importance, top = 10, col = "blue",  main = "Random Forest Classification")

