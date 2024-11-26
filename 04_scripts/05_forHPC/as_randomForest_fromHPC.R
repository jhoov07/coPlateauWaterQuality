# This script will take execute random forest for an input dataset
# such database will be outputted in wide or narrow format
# Author: Mery Touceda-Suarez (mtoucedasuarez@arizona.edu) and Joe Hoover (jhoover@arizona.edu)
# Date: July 2024

# USAGE: 
# Rscript [PATH-TO-SRIPT]/create_db_from_csvs.R [IN-PATH] [OUT-PATH] [DB-NAME]
# Example:
# Rscript Desktop/step2_rf_as_modelProcessing_forHPC.R Downloads/data Documents/waterproject/database indigenousWaterQuality


# SETUP
#Load and install packages

#install.packages("caTools")
#install.packages("randomForest")
#install.packages("caret")
#install.packages("tidyverse")

# load libraries

library(caTools)
library(randomForest)
library(caret)
library(tidyverse)

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
Asdata = read.csv(in_path, na.strings = "NA")

#Confirm complete cases for input data
Asdata <- Asdata[complete.cases(Asdata), ]

#Subset to training set
AsTrain<-subset(Asdata, trainCat3==TRUE)

#Drop unused fields
AsTrain<-train[,-c(1, 4, 109:112, 157, 159:168)] #Drop the As concentration, and the categorical variables we already transformed


#Ensure As3Cat is a Factor (Categorical Variable)
AsTrain$As3Cat <- as.factor(AsTrain$As3Cat)

#Drop unused fields
AsTrain<-AsTrain[,-c(1:6,213:215, 217)]

#Set a tune grid
n<-ncol(AsTrain)-1
tunegrid <- expand.grid(mtry = 1:n)

# Fitting Random Forest to the train dataset 
  classifier_RF<-train(
    data = AsTrain, 
    factor(As3Cat) ~ .,
    metric = "Accuracy",
    method = "rf",
    trControl = trainControl(method="cv", number = 10, verboseIter = TRUE),    #change number = 10 if doing for real
    tuneGrid  = tunegrid,
    ntree = 500)  # Enable verbose output for troubleshooting

#Save model as an object file
wide_out_name <- paste(date, "rf.rds", sep = "_")
wide_out_path <- paste(out_path, wide_out_name, sep="/")
saveRDS(classifier_RF, wide_out_path)