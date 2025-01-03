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
alpha=args[3]
labmda=args[4]
gamma=args[5]
max_delta_step=args[6]
min_child_weight=args[7]

eta = args[8]  #Shrinkage Step size shrinkage used in update to prevent overfitting. After each boosting step, we can directly get the weights of new features, and eta shrinks the feature weights to make the boosting process more conservative.
max_depth = args[9] #Maximum depth of a tree. Increasing this value will make the model more complex and more likely to overfit. 0 indicates no limit on depth. 
nrounds = args[10]
subsample = args[11] #Subsample ratio of the training instances. Setting it to 0.5 means that XGBoost would randomly sample half of the training data prior to growing trees and this will prevent overfitting. 
colsample_bytree = args[12]


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
AsTrain<-train[,-c(1, 4, 109:112, 158:168)] #Drop the As concentration, and the categorical variables we already transformed

#Ensure ClassLTE2 is a Factor (Categorical Variable)
AsTrain$ClassLTE1 <- as.factor(AsTrain$ClassLTE1)

#Variables for file name
outcomeM<-"ClassLTE10"
cv<-"cv10"

#Set a tune grid
#n<-ncol(AsTrain)-1
#tunegrid <- expand.grid(mtry = 1:n)

#This model takes ~5 minutes to run on my laptop
model<-train(
  factor(ClassLTE1) ~ ., 
  data = AsTrain, 
  metric = "Accuracy",
  method = "xgbTree",
  trControl = trainControl(method="cv", number = 10, verboseIter = TRUE),
  na.action = 'na.pass',
  tuneGrid = expand.grid(
    eta = eta,  #Shrinkage Step size shrinkage used in update to prevent overfitting. After each boosting step, we can directly get the weights of new features, and eta shrinks the feature weights to make the boosting process more conservative.
    max_depth = max_depth, #Maximum depth of a tree. Increasing this value will make the model more complex and more likely to overfit. 0 indicates no limit on depth. 
    nrounds = nrounds,
    subsample = subsample, #Subsample ratio of the training instances. Setting it to 0.5 means that XGBoost would randomly sample half of the training data prior to growing trees and this will prevent overfitting. 
    colsample_bytree = colsample_bytree, #is the subsample ratio of columns when constructing each tree. Subsampling occurs once for every tree constructed.
    
    max_delta_step = max_delta_step, #Maximum delta step we allow each leaf output to be. If the value is set to 0, it means there is no constraint. If it is set to a positive value, it can help making the update step more conservative. Usually this parameter is not needed, but it might help in logistic regression when class is extremely imbalanced.
    gamma = gamma, #Minimum loss reduction required to make a further partition on a leaf node of the tree. The larger gamma is, the more conservative the algorithm will be.
    alpha = alpha, #L1 regularization term on weights. Increasing this value will make model more conservative
    lambda = labmda, #L2 regularization term on weights. Increasing this value will make model more conservative
    min_child_weight = min_child_weight, #Minimum sum of instance weight (hessian) needed in a child. If the tree partition step results in a leaf node with the sum of instance weight less than min_child_weight, then the building process will give up further partitioning. 
    
  )
)

#Save model as an object file
rds_out_name <- paste(date, outcomeM, cv, ,"xgb.rds", sep = "_")
rds_out_path <- paste(out_pathDir, rds_out_name, sep="/")
saveRDS(model, rds_out_path)