# This script will take execute extreme gradient boosting for an input dataset
# such database will be outputted in wide or narrow format
# Author: Aaron Nuanez (aaronnuanez@arizona.edu) and Joe Hoover (jhoover@arizona.edu)
# Date: July 2024

# USAGE: 
# Rscript [PATH-TO-SRIPT]/cas_XGB10ugL.R [IN-PATH] [OUT-PATHDIR] [OUT-NAME]
# Example:
# Rscript home/u29/aaronnuanez/arsenic/scripts/as_XGB_10ugL.R /home/u29/aaronnuanez/arsenic/data/2_20240724_randomForest_As_dataClean.csv /home/u29/aaronnuanez/arsenic/data/ /home/u29/aaronnuanez/arsenic/data/xgb_as_model


# SETUP
#Load and install packages

#install.packages("caTools")
#install.packages("randomForest")
#install.packages("caret")
#install.packages("tidyverse")

# load libraries

library(caTools) 
library(caret)
library(gbm)
library(xgboost) # for xgboost
#library(tidyverse) # general utility functions

# load arguments
args<-commandArgs(TRUE)

## Input files and directories
in_path=args[1]
out_pathDir=args[2]
tune_var=args[3]
gamma=args[4]
min_child_weight=args[5]

print(in_path)
print(out_pathDir)
#print(alpha)
#print(lambda)
print(gamma)
#print(max_delta_step)
print(min_child_weight)

# set data and seed values
date<-Sys.Date()
set.seed(1234)  # Setting seed 

#Load data
Asdata = read.csv(in_path, na.strings = "NULL")

# Filter data into train and test sets based on logical variable
train <- Asdata[Asdata$trainClassLTE10_splt == TRUE, ] 

#Make SiteID the row name so we can drop that field
rownames(train)<-train$SiteID

#Drop unused fields
#AsTrain<-train[,-c(1, 4, 109:112, 158:168)] #LTE1
#AsTrain<-train[,-c(1, 4, 109:112, 157, 159:168)] #LTE2
#AsTrain<-train[,-c(1, 4, 109:112, 157:158, 160:168)] #LTE3
#AsTrain<-train[,-c(1, 4, 109:112, 157:159, 161:168)] #LTE5
AsTrain<-train[,-c(1, 4, 109:112, 157:160, 162:168)] #LTE10

#Ensure ClassLTE2 is a Factor (Categorical Variable)
AsTrain$ClassLTE10 <- as.factor(AsTrain$ClassLTE10)

#Variables for file name
outcomeM<-"ClassLTE10"
cv<-"cv10"

#Set a tune grid
#n<-ncol(AsTrain)-1
#tunegrid <- expand.grid(mtry = 1:n)

#This model takes ~5 minutes to run on my laptop
model<-train(
  factor(ClassLTE10) ~ ., 
  data = AsTrain, 
  metric = "Accuracy",
  method = "xgbTree",
  trControl = trainControl(method="cv", number = 10, verboseIter = TRUE),
  na.action = 'na.pass',
  tuneGrid = expand.grid(
    eta = 0.005,
    max_depth = 6,
    nrounds = 750,
    subsample = 0.5,
    colsample_bytree = 0.75,
    gamma = gamma,
    min_child_weight = min_child_weight
 )
)

#Save model as an object file
rds_out_name <- paste(date, outcomeM, cv, tune_var, "xgb.rds", sep = "_")
rds_out_path <- paste(out_pathDir, rds_out_name, sep="/")
saveRDS(model, rds_out_path)