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

#Make a list of the fewest number of variables with the highest overall prediction accuracy
#highest accuracy is 0.768 using 12 variables with the highest gain values - from the csv output from step 2
a<-list("pH","Fe","A_Calcite","prism30yr","DepthToGW","C_Sb","A_Kaolinit",
        "C_Tot_14A","C_Hematite","Top5_Ca","A_Tot_Flds","C_Se")
a

#define predictor and response variables in training set, As= 5 ug/L, keep variables defined above
train_x = data.matrix(train[, c(1, 3, 2, 27,5, 108,87,38,106, 99,11,60,88)])
train_y = train[,160]

#define predictor and response variables in testing set
test_x = data.matrix(train[, c(1, 3, 2, 27,5, 108,87,38,106, 99,11,60,88)])
test_y = test[, 160]

#define final training and testing sets
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

#define watchlist
watchlist = list(train=xgb_train, test=xgb_test)

#Set parameters from all the tuning
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

#Fully tuned model
model = xgboost(data = xgb_train, params = params,
                   nrounds = 750, objective = "binary:logistic",
                   eval_metric = "error", verbose = 1,
                   print_every_n = 100)
  

#write.csv(dfAc, file="20241223_modelTuning_primaryHyperparameters_alpha2Lambda5.csv")


#Testing Data
xgbpred <- predict (model, xgb_test)
xgbpred2 <- ifelse (xgbpred > 0.5,1,0)
confusionMatrix (factor(xgbpred2), factor(test_y))

#Adjust the "true" threshold using Youden value
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
