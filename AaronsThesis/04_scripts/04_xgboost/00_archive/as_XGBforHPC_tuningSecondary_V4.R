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

# load arguments
#args<-commandArgs(TRUE)

## Input files and directories
#in_path=args[1]
#out_pathDir=args[2]
#tune_var=args[3]
#gamma=args[4]
#min_child_weight=args[5]

#print(in_path)
#print(out_pathDir)
#print(alpha)
#print(lambda)
#print(gamma)
#print(max_delta_step)
#print(min_child_weight)

rm(list=ls())

# set data and seed values
date<-Sys.Date()
set.seed(1234)  # Setting seed 

setwd("/Users/hoover/Documents/GitHub/coPlateauWaterQuality/03_data/")
#Load data
#Asdata = read.csv(in_path, na.strings = "NULL")
Asdata = read.csv("All_As_Data.csv", na.strings = "NULL")

# Filter data into train and test sets based on logical variable
train <- Asdata[Asdata$trainClassLTE10_splt == TRUE, ] 
test <- Asdata[Asdata$trainClassLTE10_splt == FALSE, ] 

#Make SiteID the row name so we can drop that field
rownames(train)<-train$SiteID
rownames(test)<-test$SiteID

#AsTrain<-train[,-c(1, 4, 109:112, 157:160, 162:168)] #LTE10

#Ensure ClassLTE2 is a Factor (Categorical Variable)
#AsTrain$ClassLTE10 <- as.factor(AsTrain$ClassLTE10)

#define predictor and response variables in training set
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

#Run model 10 times and calculate accuarcy and SD of accuracy, change hyperparameter value as needed
dfAc<-data.frame()
params = list(alpha = 0,
              lambda = 5,
              gamma = 0,
              max_delta_step = 0,
              eta = 0.005,
              max_depth = 6,
              subsample = 0.50,
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
  xy<-cbind(x,y)
  dfAc<-rbind(dfAc, xy)
}

#Clean up and write to file
colnames(dfAc)[1]<-"Train_Error"
colnames(dfAc)[2]<-"Test_Error"
mean(dfAc$Train_Error)
sd(dfAc$Train_Error)
mean(dfAc$Test_Error)
sd(dfAc$Test_Error)

write.csv(dfAc, file="20241223_modelTuning_primaryHyperparameters_alpha2Lambda5.csv")



#Testing Data
xgbpred <- predict (model, xgb_test)
xgbpred2 <- ifelse (xgbpred > 0.5,1,0)
confusionMatrix (factor(xgbpred2), factor(test_y))

#Fully tuned model
modelF = xgboost(data = xgb_train, params = params,
                  nrounds = 750, objective = "binary:logistic",
                  eval_metric = "error", verbose = 1,
                  print_every_n = 100)

#Determine variable importance
# Compute feature importance matrix
importance_matrix = xgb.importance(colnames(xgb_train), model = model)
head(importance_matrix)

importance_matrix$realtiveGain<-importance_matrix$Gain/0.0759
importance_matrix$filter<- 1
importance_matrix$filter[importance_matrix$realtiveGain<0.25]<- 0

# Nice graph
xgb.plot.importance(importance_matrix[1:150,])


#importance <- varImp(model, scale = TRUE)

# Plot variable importance
#plot(importance, top = 150, col = "blue",  main = "Variable Importance, XGB, As > 3ug/L")


#Convert data frame to long form, filter and then rerun XGB model
#Asdata = read.csv(in_path, na.strings = "NULL")
library(reshape2)
library(tidyverse)

#Remove low importance columns for training
trainL<-melt(train_x)
colnames(trainL)[1]<-"SiteId"
colnames(trainL)[2]<-"Feature"
trainL2<-merge(trainL, importance_matrix, by="Feature")

trainData2<-trainL2 %>%
  filter(filter == 1)

train_x2<-dcast(trainData2[,c(1:3)], SiteId~Feature, value.var = "value")
train_x3<-data.matrix(train_x2)

#Remove low importance columns for testing
testL<-melt(test_x)
colnames(testL)[1]<-"SiteId"
colnames(testL)[2]<-"Feature"
testL2<-merge(testL, importance_matrix, by="Feature")

testData2<-testL2 %>%
  filter(filter == 1)

test_x2<-dcast(testData2[,c(1:3)], SiteId~Feature, value.var = "value")
test_x3<-data.matrix(test_x2)

#define final training and testing sets
xgb_train3 = xgb.DMatrix(data = train_x3, label = train_y)
xgb_test3 = xgb.DMatrix(data = test_x3, label = test_y)

#define watchlist
watchlist3 = list(train=xgb_train3, test=xgb_test3)

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

##XGB Train
modelReduced = xgb.train(data = xgb_train3, params = params,
                  watchlist = watchlist3,
                  nrounds = 750, objective = "binary:logistic",
                  eval_metric = "error", verbose = 1,
                  print_every_n = 100)
#Testing Data
xgbpredRed <- predict (modelReduced, xgb_test3)
xgbpredRed2 <- ifelse (xgbpredRed > 0.5,1,0)
confusionMatrix (factor(xgbpredRed2), factor(test_y))





summary(model)

model3 <- xgb.cv(data = xgb_train, nrounds = 750,
                 params = params,
                 objective = "binary:logistic",
                 metrics = "error", verbose = 1, nfold=10, print_every_n = 50,
                 showsd = T, prediction = T)
#Testing Data
xgbpred <- predict (model3, xgb_test)
xgbpred2 <- ifelse (xgbpred > 0.5,1,0)
confusionMatrix (factor(xgbpred2), factor(test_y))

#1-last(model3$evaluation_log$train_error_mean)
1-mean(model3$evaluation_log$test_error_mean)
#print(model3)

#1-mean(model$evaluation_log$test_error)

1-min(model$evaluation_log$train_error)
#1-min(model$evaluation_log$test_error)

#Training Date
#xgbpredTrain <- predict (model, xgb_train)
#xgbpredTrain <- ifelse (xgbpredTrain > 0.5,1,0)
#confusionMatrix (factor(xgbpredTrain), factor(train_y))



#For a figure
y_predJoin<-data.frame(cbind(test_y, xgbpred))#change field to match outcome modeled, this applies to LT10

#rename fields for ease of use
colnames(y_predJoin)[1]<-"Obsclass"
colnames(y_predJoin)[2]<-"Predexceed"

#Use cutpoint to identify threshold for As 'detection' balancing sensitivity and specificity using Youden metric
cp <- cutpointr(y_predJoin, Predexceed, Obsclass, 
                method = maximize_metric, metric = youden)
summary(cp)
plot(cp)

#Extract ROC Curve data for plotting
a<-as.data.frame(cp$roc_curve)
a$sens<-a$tp/(a$tp+a$fn) #sensitivity
a$spec<-a$tn/(a$tn+a$fp) #specificity
a$j<-(a$tp/(a$tp+a$fn))+(a$tn/(a$tn+a$fp))-1 #j-index, also called Youden value

##Make a plot like USGS PFAS paper S8
df <- a %>%
  select(x.sorted, j, sens, spec) %>%
  gather(key = "variable", value = "value", -x.sorted)

ggplot(df, aes(x = x.sorted, y = value)) + 
  geom_line(aes(color = variable, linetype = variable)) + 
  scale_color_manual(values = c("black","darkred", "steelblue")) +
  xlab("As Detection Threshold - value above this threshold is considered a detection") + ylab("Metric Estimate")


# To prepare the long-format data:
shap_long <- shap.prep(xgb_model = model, X_train = train_x)
# **SHAP summary plot**
shap.plot.summary(shap_long)



xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 79, 
                   watchlist = list(val=dtest,train=dtrain), print.every.n = 10, 
                   early.stop.round = 10, maximize = F , eval_metric = "error")

model2 <- xgboost(data = xgb_train, params = params,
                  nrounds = 750, objective = "binary:logistic",
                  metrics = list("error","auc"), verbose = 1, print_every_n = 50)

model3 <- xgb.cv(data = xgb_train, nrounds = 1000,
                params = params,
                objective = "binary:logistic",
                metrics = "error", verbose = 1, nfold=10, print_every_n = 50,
                showsd = T, stratified = T)
#print(model3, verbose = TRUE)

1-min(model3$evaluation_log$train_error_mean)
1-min(model3$evaluation_log$test_error_mean)


xgbpred <- ifelse (xgbpred > 0.5,1,0)


