#Load and install packages

#install.packages("caTools")
#install.packages("randomForest")
#install.packages("caret")
#install.packages("tidyverse")

#Call packages
library(caTools) 
library(randomForest)
library(caret)
library(tidyverse)

#Set working directory
setwd("/Users/hoover/Documents/GitHub/coPlateauWaterQuality/02_uranium")
#setwd("/Users/austinmartinez/Documents/GitHub/coPlateauWaterQuality/01_data")

#Clean up the workspace
rm(list=ls())

date<-Sys.Date()
set.seed(1234)  # Setting seed 

#Load data
data = read.csv("./01_data/Cleaned_U_GIS_Filtered_Joe.csv",
                  na.strings = c("NULL", "NA")) #Probably need to simplify the path so the script and data are in the same folder for the HPC

data <- data[complete.cases(data), ]

#Subset to training set
train<-subset(data, trainCat3==TRUE)


#Ensure As3Cat is a Factor (Categorical Variable)
train$U3Cat <- as.factor(train$U3Cat)

#Ensure bU30 is a Factor (Categorical Variable)
train$bU30 <- as.factor(train$bU30)

#Drop unused fields
train<-train[,-c(1:5,208:215, 217:218)]

#Set a tune grid
n<-ncol(train)-1
#tunegrid <- expand.grid(mtry = 1:n) 
tunegrid <- expand.grid(mtry = 1:5) #Change to 1:206 if testing for real, 1:3 was used for model development

# Fitting Random Forest to the train dataset 
#classifier_RF 
  classifier_RF<-train(
    data = train, 
    factor(bU30) ~ .,
    metric = "Accuracy",
    method = "rf",
    trControl = trainControl(method="cv", number = 10),    #change number = 10 if doing for real
    tuneGrid  = tunegrid,
    ntree = 500,
    verboseIter = TRUE)  # Enable verbose output for troubleshooting

saveRDS(classifier_RF, paste("./03_modelOuput/",date, "_mtry5_cv10_bU30_final_model_rf.rds", sep=""))
