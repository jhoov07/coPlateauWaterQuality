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
library("SHAPforxgboost")
library(data.table)
#library(tidyverse) # general utility functions

rm(list=ls())

# set data and seed values
date<-Sys.Date()
set.seed(1234)  # Setting seed 

#setwd("/Users/hoover/Documents/GitHub/coPlateauWaterQuality/03_data/")
setwd("/Users/aaronnuanez/Documents/GitHub/coPlateauWaterQuality/03_data/")

#Load data
#Asdata = read.csv(in_path, na.strings = "NULL")
Asdata = read.csv("All_As_Data.csv", na.strings = "NULL")

# Filter data into train and test sets based on logical variable
train <- Asdata[Asdata$trainClassLTE10_splt == TRUE, ] 
test <- Asdata[Asdata$trainClassLTE10_splt == FALSE, ] 

#Make SiteID the row name so we can drop that field
rownames(train)<-train$SiteID
rownames(test)<-test$SiteID

#Make a list of the fewest number of variables with the highest overall prediction accuracy and KEEP SITEID
#highest accuracy is 0.850 using 10 variables with the highest gain values - from the csv output from step 3
a<-list("pH", "prism30yr", "A_Cs", "A_Aragon", "C_Hematite", "Fe", "Top5_S", "C_Cr", "A_Calcite", "DepthToGW", "C_Mo", "Top5_Ca", "A_Tot_14A", "C_Amorph", "C_Analcime")
        
#Variables to be added for testing 15 total variables with an accuracy of 0.850 ("C_Mo", "Top5_Ca", "A_Tot_14A", "C_Amorph", "C_Analcime") (80, 11, 58, 65, 66)      

#define predictor and response variables in training set, As= 10 ug/L, keep variables defined above
train_x = data.matrix(train[, c(3, 5, 29, 25, 99, 2, 17, 71, 27, 108, 80, 11, 58, 65, 66)])
#train_x = data.matrix(train[, -c(1, 4, 109:112, 157:168)])
train_y = train[,161]

#define predictor and response variables in testing set
test_x = data.matrix(test[, c(3, 5, 29, 25, 99, 2, 17, 71, 27, 108, 80, 11, 58, 65, 66)])
test_y = test[, 161]

#define final training and testing sets
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

#define watchlist
watchlist = list(train=xgb_train, test=xgb_test)

#Use fully tuned hyperparameters from steps 1 and 2
dfAc<-data.frame()
params = list(alpha = 2,
              lambda = 5,
              gamma = 1,
              max_delta_step = 1,
              eta = 0.005,
              max_depth = 6,
              subsample = 0.5,
              colsample_bytree = 0.75,
              min_child_weight = 1,
              booster = "gbtree")

##XGB Train
for(data in 1:10){
  model = xgb.train(data = xgb_train, params = params,
                    watchlist = watchlist,
                    nrounds = 750, objective = "binary:logistic",
                    eval_metric = list("error"), verbose = 1,
                    print_every_n = 100)
  
  
  x<-1-last(model$evaluation_log$train_error)
  y<-1-last(model$evaluation_log$test_error)
  xy<-cbind(x,y); print(xy)
  dfAc<-rbind(dfAc, xy)
}

#Clean up and write to file
colnames(dfAc)[1]<-"Train_Error"
colnames(dfAc)[2]<-"Test_Error"
mean(dfAc$Train_Error)
sd(dfAc$Train_Error)
mean(dfAc$Test_Error)
sd(dfAc$Test_Error)

#write.csv(dfAc, file="20241223_as5ugL_modelTuning_primaryHyperparameters.csv")

#Testing Data
xgbpred <- predict (model, xgb_test)
xgbpred2 <- ifelse (xgbpred > 0.5,1,0)
confusionMatrix (factor(xgbpred2), factor(test_y))

#Compare training and testing accuracy to the model with all variables and tuned hyperparameters - from Step 2
#Rerun with different variable subset if needed, might take some tinkering to identify the correct number of variables to keep
#Make note of the variables to keep then go to script 5 and run the final model and calculate model metrics


