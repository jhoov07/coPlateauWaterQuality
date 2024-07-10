library(caTools) 
library(caret)
library(gbm)
library(xgboost) # for xgboost
library(tidyverse) # general utility functions
library(smotefamily) #use to balance the training dataset

#setwd("/Users/hoover/Documents/GitHub/coPlateauWaterQuality/01_data")
setwd("/Users/austinmartinez/Documents/GitHub/coPlateauWaterQuality/01_data")

#Clean up the workspace
rm(list=ls())

#Load data
All_Asdata = read.csv("AsModelInput.csv")

# set a random seed & shuffle data frame
set.seed(1234)
Asdata <- All_Asdata[sample(1:nrow(All_Asdata)), ]
#Asdata<-as.data.frame(Asdata)

# get the numb 70/30 training test split
#split into training (70%) and testing set (30%)
sample_set<-sample(nrow(Asdata), round(nrow(Asdata)*.7), replace = F)
As_train = Asdata[sample_set,]
As_test = Asdata[-sample_set,]

#Complete cases
As_trainComp <- As_train[complete.cases(As_train[,c(2,3,9:93)]),]  #col 2,3, testing on As >5/10 ug/L category
As_testComp<- As_test[complete.cases(As_test[,c(2,3,9:93)]),] #col 2,3, testing on As >5/10 ug/L category

As_trainComp <- As_trainComp[,c(2,3,9:93)] #col 2,3, testing on As >5/10 ug/L category
As_testComp<- As_testComp[,c(2,3,9:93)] #col 2,3, testing on As >5/10 ug/L category

#define predictor and response variables in training set
train_x<-data.matrix(As_trainComp[, -c(1:8)])
train_y<-As_trainComp[,2,3]

#define predictor and response variables in testing set
test_x<-data.matrix(As_testComp[, -c(1:8)])
test_y<-As_testComp[,2,3]

#define final training and testing sets
#xgb_train = xgb.DMatrix(data = train_x, label = train_y)  #OK, this runs when the outcome variable is numeric
#xgb_test = xgb.DMatrix(data = test_x, label = test_y)

#define watchlist
#watchlist = list(train=xgb_train, test=xgb_test)

#fit XGBoost model and display training and testing data at each round
#model = xgb.train(data = xgb_train, max.depth = 4, watchlist=watchlist, nrounds = 70, objective = "binary:logistic")


#This model takes ~5 minutes to run
model<-train(
  factor(bas5 + bas10) ~ ., 
  data = As_trainComp, 
  metric = "Accuracy",
  method = "xgbTree",
  trControl = trainControl(method="cv", number = 3),
  tuneGrid = expand.grid(
    #nrounds = seq(from = 500, to = 2000, by = 500),
    nrounds = 20,
    max_depth = c(6, 8, 10),
    eta = c(0.01, 0.02),  #Shrinkage
    gamma = 0,
    colsample_bytree = c(0.5, 0.75),
    min_child_weight = 1,
    subsample = c(0.5, 0.75)
  )
)

#This model took ~40 minutes to run on my laptop, can still tune a few other parameters, you could try increasing the number to 5 or 10 but that will add lots more time. Might be best to run on a desktop in the lab.  
model<-train(
  factor(bas5 + bas10) ~ ., 
  data = As_trainComp, 
  metric = "Accuracy",
  method = "xgbTree",
  trControl = trainControl(method="cv", number = 3),
  tuneGrid = expand.grid(
    nrounds = seq(from = 500, to = 2000, by = 500),
    max_depth = seq(from = 8, to = 14, by = 2),
    eta = seq(from = 0.005, to = 0.0125, by = 0.0025),  #Shrinkage
    gamma = 0,
    colsample_bytree = seq(from = 0.25, to = 0.75, by = 0.25),
    min_child_weight = 1,
    subsample = seq(from = 0.25, to = 0.75, by = 0.25)
  )
)

model

model$resample %>%
  arrange(Resample)
