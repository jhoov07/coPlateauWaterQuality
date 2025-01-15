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

setwd("/Users/hoover/Documents/GitHub/coPlateauWaterQuality/03_data/")
#Load data
#Asdata = read.csv(in_path, na.strings = "NULL")
Asdata = read.csv("All_As_Data.csv", na.strings = "NULL")

# Filter data into train and test sets based on logical variable
train <- Asdata[Asdata$trainClassLTE5_splt == TRUE, ] 
test <- Asdata[Asdata$trainClassLTE5_splt == FALSE, ] 

#Make SiteID the row name so we can drop that field
rownames(train)<-train$SiteID
rownames(test)<-test$SiteID

#define predictor and response variables in training set, As= 5 ug/L, keep variables defined above
train_x = data.matrix(train[, -c(1, 4, 109:112, 157:168)])
train_y = train[,160] #change this field index if you change the outcome of interest

#define predictor and response variables in testing set
train_y = data.matrix(test[, -c(1, 4, 109:112, 157:168)])
test_y = test[, 160] #change this field index if you change the outcome of interest

#define final training and testing sets
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

#define watchlist
watchlist = list(train=xgb_train, test=xgb_test)

#Input tuned primary and secondary hyperparameters from Steps 1 and 2
dfAc<-data.frame()
params = list(alpha = 0,
              lambda = 1,
              gamma = 0,
              max_delta_step = 0,
              eta = 0.01,
              max_depth = 4,
              subsample = 0.5,
              colsample_bytree = 0.75,
              min_child_weight = 1,
              booster = "gbtree")

##XGB Train
model = xgb.train(data = xgb_train, params = params,
                    watchlist = watchlist,
                    nrounds = 1000, objective = "binary:logistic",
                    eval_metric = list("error"), verbose = 1,
                    print_every_n = 100)

# Compute feature importance matrix from model with tuned hyperparameters
importance_matrix = xgb.importance(colnames(xgb_train), model = model)
head(importance_matrix)

# Nice graph of variable importance
xgb.plot.importance(importance_matrix)

## reorder attribute tablef rom most to least important variable, then create a new dataframe
library(tidyverse)
qqq <- importance_matrix %>%
  arrange(Gain)  # arrange in descending order

head(qqq)

#Create a filter order using the ordered list of variables in qqq dataframe
qqq$filterID<-seq(length(qqq$Gain))

#Load libraries to do some data frame reorganization
library(reshape2)
library(tidyverse)

#Make data training data long format for easy filtering
#length(qqq$Gain) this gives a count of the number of variables in the original data frame for the loop to iterate through
dfMetrics<-data.frame()
for (i in 1:length(qqq$Gain)){
  trainL<-melt(train_x) #convert to long format
  colnames(trainL)[1]<-"SiteId"
  colnames(trainL)[2]<-"Feature"
  trainL2<-merge(trainL, qqq, by="Feature") #merge with dataframe that has variable importance ranks
  
  #Filter based on iteration
  trainData2<-trainL2 %>%
    filter(filterID > 0+i) #remove 0+i lowest importance variables. i is the loop iteration
  
  #Convert back to wide format
  train_x2<-dcast(trainData2[,c(1:3)], SiteId~Feature, value.var = "value")
  train_x3<-data.matrix(train_x2)
  
  #Prep test data so you work with the same number of variables as the training data
  testL<-melt(test_x)
  colnames(testL)[1]<-"SiteId"
  colnames(testL)[2]<-"Feature"
  testL2<-merge(testL, qqq, by="Feature")
  
  testData2<-testL2 %>%
    filter(filterID >=0+i)
  
  test_x2<-dcast(testData2[,c(1:3)], SiteId~Feature, value.var = "value")
  test_x3<-data.matrix(test_x2)
  
  #define  training and testing sets
  xgb_train3 = xgb.DMatrix(data = train_x3, label = train_y)
  xgb_test3 = xgb.DMatrix(data = test_x3, label = test_y)
  
  #define watchlist
  watchlist3 = list(train=xgb_train3, test=xgb_test3)
  
  #use fully tuned primary and secondary hyperparameters from steps 1 and 2
  params = list(alpha = 0,
                lambda = 1,
                gamma = 0,
                max_delta_step = 0,
                eta = 0.005,
                max_depth = 6,
                subsample = 0.50,
                colsample_bytree = 0.75,
                min_child_weight = 1,
                booster = "gbtree")
  
  dfAd<-data.frame()
  ##XGB Train
  for(w in 1:5){
    model = xgb.train(data = xgb_train, params = params,
                      watchlist = watchlist,
                      nrounds = 750, objective = "binary:logistic",
                      eval_metric = list("error"), verbose = 1,
                      print_every_n = 100)
    
    
    x<-1-last(model$evaluation_log$train_error)
    y<-1-last(model$evaluation_log$test_error)
    xy<-cbind(x,y)
    dfAd<-rbind(dfAd, xy)
  }
  
  #Clean up and write to file
  #colnames(dfAc)[1]<-"Train_Error"
  #colnames(dfAc)[2]<-"Test_Error"
  ab<-mean(dfAd[[1]])
  ac<-sd(dfAd[[1]])
  ad<-mean(dfAd[[2]])
  ae<-sd(dfAd[[2]])
  abc<-cbind(i,ab,ac,ad,ae)
  dfMetrics<-rbind(dfMetrics, abc)
  print(i)
  
}

colnames(dfMetrics)[2]<-"Train_Error"
colnames(dfMetrics)[3]<-"Train_SD"
colnames(dfMetrics)[4]<-"Test_Error"
colnames(dfMetrics)[5]<-"Test_SD"
#colnames(dfAc)[2]<-"Test_Error"

#write.csv(dfMetrics, file="20241226_as5XGB_variableDrop_accuracySDImpacts.csv")


#Make a plot to show how accuracy varies by number of variables
ggplot(dfMetrics, aes(x=i, y=Test_Error))+geom_line()

#Evaluate the plot and identify any changes you observed between number of variables included and overall accuracy. 
#Try to find the best accuracy with the least number of variables
#Make note of the variables and then go to step 4 to run the reduced variable model

