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


#define predictor and response variables in training set, As= 10 ug/L, keep variables defined above
train_x = data.matrix(train[, -c(1, 4, 109:112, 157:168)])
train_y = train[,161]

#define predictor and response variables in testing set
test_x = data.matrix(test[, -c(1, 4, 109:112, 157:168)])
test_y = test[, 161]

#define final training and testing sets
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

#define watchlist
watchlist = list(train=xgb_train, test=xgb_test)

#Run the model with tuned parameters
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
model = xgb.train(data = xgb_train, params = params,
                    watchlist = watchlist,
                    nrounds = 750, objective = "binary:logistic",
                    eval_metric = list("error"), verbose = 1,
                    print_every_n = 100)
  

# Compute feature importance matrix
importance_matrix = xgb.importance(colnames(xgb_train), model = model)
head(importance_matrix)

# Nice graph
xgb.plot.importance(importance_matrix[c(1:25)])

##
library(tidyverse)
qqq <- importance_matrix %>%
  arrange(Gain)  # arrange in descending order

head(qqq)

#importanceList<-data.frame(lapply(importance_matrix, sort))
qqq$filterID<-seq(length(qqq$Gain))

library(reshape2)
library(tidyverse)

#Make data training data long format for easy filtering
dfMetrics<-data.frame()
for (i in 1:length(qqq$Gain)){
  trainL<-melt(train_x)
  colnames(trainL)[1]<-"SiteId"
  colnames(trainL)[2]<-"Feature"
  trainL2<-merge(trainL, qqq, by="Feature")
  
  #Filter based on iteration
  trainData2<-trainL2 %>%
    filter(filterID > 0+i)
  
  #Convert back to wide format
  train_x2<-dcast(trainData2[,c(1:3)], SiteId~Feature, value.var = "value")
  train_x3<-data.matrix(train_x2)
  
  #Prep test data
  testL<-melt(test_x)
  colnames(testL)[1]<-"SiteId"
  colnames(testL)[2]<-"Feature"
  testL2<-merge(testL, qqq, by="Feature")
  
  testData2<-testL2 %>%
    filter(filterID >=0+i)
  
  test_x2<-dcast(testData2[,c(1:3)], SiteId~Feature, value.var = "value")
  test_x3<-data.matrix(test_x2)
  
  #define final training and testing sets
  xgb_train3 = xgb.DMatrix(data = train_x3, label = train_y)
  xgb_test3 = xgb.DMatrix(data = test_x3, label = test_y)
  
  #define watchlist
  watchlist3 = list(train=xgb_train3, test=xgb_test3)
  
  params = list(alpha = 2,
                lambda = 5,
                gamma = 1,
                max_delta_step = 1,
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

write.csv(dfMetrics, file="~/Desktop/2025122_as10XGB_variableDrop_accuracySDImpacts.csv")


#Make a plot to show how accuracy varies by number of variables
ggplot(dfMetrics, aes(x=i, y=Test_Error))+geom_line()

