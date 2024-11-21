# This script will take execute random forest for an input dataset
# such database will be outputted in wide or narrow format
# Author: Aaron Nuanez (aaronnuanez@arizona.edu) and Joe Hoover (jhoover@arizona.edu)
# Date: November 2024

# USAGE: 
# Rscript [PATH-TO-SRIPT]/create_db_from_csvs.R [IN-PATH] [OUT-PATH] [DB-NAME]
# Example:
# Rscript Desktop/step2_rf_as_modelProcessing_forHPC.R Downloads/data Documents/waterproject/database indigenousWaterQuality


# SETUP
#Load and install packages
library(caTools) 
library(randomForest)
library(caret)
library(tidyverse)


#setwd("/Users/hoover/Documents/GitHub/coPlateauWaterQuality/03_data/")
setwd("/Users/aaronnuanez/Documents/GitHub/coPlateauWaterQuality/03_data")

rm(list=ls())

# load arguments
args<-commandArgs(TRUE)

## Input files and directories
in_path=args[1]
out_path=args[2]
#out_name=args[3]

# set data and seed values
date<-Sys.Date()
set.seed(1234)  # Setting seed 

#Load data
Asdata <- read.csv("All_As_Data.csv")

#Confirm complete cases for input data
#Asdata <- Asdata[complete.cases(Asdata), ]

# get the numb 70/30 training test split
#split into training (70%) and testing set (30%), keep training set balances with overall distribution
sample_set<-sample.split(Asdata$ClassLTE1, SplitRatio = 0.7)

Asdata2 <- Asdata %>%
  mutate(
    trainCat2 = ifelse(sample_set == TRUE, 1, 0)
  )


# Filter data into train and test sets based on logical variable 'trainCat2'
train <- Asdata2[Asdata2$trainCat2 == TRUE, ] #Need up update this field and dataframe to match what is produce in lines 21-24
test <- Asdata2[Asdata2$trainCat2 == FALSE, ] #Need up update this field and dataframe to match what is produce in lines 21-24

#Make SiteID the row name so we can drop that field
rownames(train)<-train$SiteID
rownames(test)<-test$SiteID

#Drop unused fields
AsTrain<-train[,-c(1, 4, 109:112, 158:163)] #Drop the As concentration, and the categorical variables we already transformed
AsTest<-test[,-c(1, 4, 109:112, 158:163)]

#Set a tune grid
n<-ncol(AsTrain)-1
tunegrid <- expand.grid(mtry = 1:n)

#Ensure ClassLTE1 is a Factor (Categorical Variable)
AsTrain$ClassLTE1 <- as.factor(AsTrain$ClassLTE1)
AsTest$ClassLTE1  <- as.factor(AsTest$ClassLTE1)

# Fitting Random Forest to the train dataset 
tunegrid <- expand.grid(mtry = (1:5)) #Change to 1:84 if testing for real, 1:3 was used for model development

#this is the more accurate model out put 
#it was ran on the super computer
#classifier_RF = readRDS("/Users/aaronnuanez/Documents/GitHub/coPlateauWaterQuality/03_modelOutputs/01_randomForest/.rds")

# Fitting Random Forest to the train dataset 
classifier_RF<-train(
  data = AsTrain,
  factor(ClassLTE1) ~ ., 
  metric = "Accuracy",
  method = "rf",
  trControl = trainControl(method="cv", number = 10),    #change number = 10 if doing for real
  tuneGrid  = tunegrid,
  ntree = 500,
  verboseIter = TRUE  # Enable verbose output for troubleshooting
)

#Save model as an object file
wide_out_name <- paste(date, "rf.rds", sep = "_")
wide_out_path <- paste(out_path, wide_out_name, sep="/")
saveRDS(classifier_RF, wide_out_path)



#classifier_RF

# Predicting the Test set results 
#y_pred <- predict(classifier_RF, newdata = AsTest)

# Confusion Matrix 
#confusion_mtx <- confusionMatrix(y_pred, AsTest$ClassLTE1)
#confusion_mtx

# Plotting model 
#plot(classifier_RF)

# Calculate Accuracy
#accuracy <- confusion_mtx$overall['Accuracy']
#accuracy

# Calculate kappa value
#kappa_value <- confusion_mtx$overall['Kappa']
#kappa_value

# Extract Sensitivity and Specificity for each class
#sensitivity <- confusion_mtx$byClass[[1]]
#sensitivity
#specificity <- confusion_mtx$byClass[[2]]
#specificity

# training data accuracy and kappa
# AvgAcc = accuracy  Avgkap = kappa
#classifier_RF$resample %>%
  #arrange(Resample) %>%
  #mutate(AvgAcc = mean(Accuracy)) %>%
  #mutate(Avgkap = mean(Kappa))


# Test data values
#accuracy
#kappa_value
#sensitivity
#specificity

#importance <- varImp(classifier_RF, scale = FALSE)

# Plot variable importance
#plot(importance, top = 10, col = "blue",  main = "Random Forest Classification")

