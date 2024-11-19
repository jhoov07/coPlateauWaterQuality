# This script will take execute extreme gradient boosting for an input dataset
# such database will be outputted in wide or narrow format
# Author: Mery Touceda-Suarez (mtoucedasuarez@arizona.edu) and Joe Hoover (jhoover@arizona.edu)
# Date: July 2024

# USAGE: 
# Rscript [PATH-TO-SRIPT]/cas_XGB10ugL.R [IN-PATH] [OUT-PATHDIR] [OUT-NAME]
# Example:
# Rscript home/u18/jhoover/arsenic/scripts/as_XGB_10ugL.R /home/u18/jhoover/arsenic/data/2_20240724_randomForest_As_dataClean.csv /home/u18/jhoover/arsenic/data/ /home/u18/jhoover/arsenic/data/xgb_as_model


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
library(tidyverse) # general utility functions

# load arguments
args<-commandArgs(TRUE)

## Input files and directories
in_path=args[1]
out_pathDir=args[2]

# set data and seed values
date<-Sys.Date()
set.seed(1234)  # Setting seed 

#Load data
Asdata = read.csv(in_path, na.strings = "NULL")

#Subset to training set
AsTrain<-subset(Asdata, spl3cat==TRUE)

#Drop unused fields for As3Cat outcome
AsTrain<-AsTrain[,-c(1:5,212:214,216:217)]

outcomeM<-"As3Cat"
cv<-"cv10"

#This model takes ~5 minutes to run on my laptop
model<-train(
  factor(As3Cat) ~ ., 
  data = AsTrain, 
  metric = "Accuracy",
  method = "xgbTree",
  trControl = trainControl(method="cv", number = 10, verboseIter = TRUE),
  na.action = 'na.pass',
  tuneGrid = expand.grid(
    nrounds = seq(from = 250, to = 1000, by = 250),
    max_depth = seq(from = 2, to = 14, by = 2),
    eta = seq(from = 0.005, to = 0.0125, by = 0.0025),  #Shrinkage
    gamma = 0,
    colsample_bytree = seq(from = 0.25, to = 0.75, by = 0.25),
    min_child_weight = 1,
    subsample = seq(from = 0.25, to = 0.75, by = 0.25)
  )
)

#Save model as an object file
rds_out_name <- paste(date, outcomeM, cv, "xgb.rds", sep = "_")
rds_out_path <- paste(out_pathDir, rds_out_name, sep="/")
saveRDS(model, rds_out_path)