library(caTools) 
library(caret)
library(gbm)
library(xgboost) # for xgboost
library(tidyverse) # general utility functions
#library(smotefamily) #use to balance the training dataset

setwd("/Users/hoover/Documents/GitHub/coPlateauWaterQuality/")
#setwd("/Users/austinmartinez/Documents/GitHub/coPlateauWaterQuality/01_data")

#Clean up the workspace
rm(list=ls())

#Load RF model
Arsenic_xgb<-readRDS("./03_modelOutputs/03_xgb/2024-07-26_As3Cat_cv5_xgb.rds")

#Load data
Asdata = read.csv("./01_data/CoPlateau_As/Cleaned_As_GIS_Filtered.csv",
                  na.strings = "NULL") #Probably need to simplify the path so the script and data are in the same folder for the HPC

#Subset to test set
AsTest<-subset(Asdata, spl3cat==FALSE)
#AsTest<-subset(Asdata, spl3cat==TRUE)

#Drop unused fields
AsTest2<-AsTest[,-c(212:214,216:217)]

AsTest3 <- as.matrix(AsTest2[complete.cases(AsTest2[,c(6:212)]), ])

#Predictions
y_pred2 = predict(Arsenic_xgb, newdata = AsTest3[,-1], type="prob") 
y_pred = predict(Arsenic_xgb, newdata = AsTest3[,-1], missing ="NULL"); length(y_pred)

#Write predictions to file for mapping
a<-cbind(AsTest3[,c(1:5)], y_pred, y_pred2)
write.csv(a, "20240726_wellPredictions_forMapping_xgb.csv")
#b<-merge(Asdata[,c(1:5)], a, by="SiteID")

# Confusion Matrix 
#confusion_mtx = table(AsTest[,216], y_pred) 
#confusion_mtx

confusionMatrix(factor(AsTest3$As3Cat), y_pred)

# Plotting model 
plot(Arsenic_xgb) 

#Calculate variable importance
gbmImp <- varImp(Arsenic_xgb, scale=F)
gbmImp

#Plot variable importance
# - Variable importance
plot(gbmImp, top=10) 


xgb.plot.shap(AsTest3, model = Arsenic_xgb, shap_contrib = NULL, trees = trees0, target_class = 0, top_n = 4,
              n_col = 2, col = col, pch = 16, pch_NA = 17)



#Set some basic parameters
date<-Sys.Date()
set.seed(1234)  # Setting seed 

#Load data
Asdata = read.csv("./CoPlateau_As/20240724_xgb_As_dataClean.csv",
                  na.strings = "NA") #Probably need to simplify the path so the script and data are in the same folder for the HPC


#Subset to training set
AsTrain<-subset(Asdata, trainCat3==TRUE)

#Drop unused fields for bas1 outcome
AsTrain<-AsTrain[,-c(1:6,214:217)]

#Ensure bas1 is a Factor (Categorical Variable)
AsTrain$As3Cat <- as.factor(AsTrain$bas1)

#This model takes ~5 minutes to run on my laptop
model<-train(
  factor(bas1) ~ ., 
  data = AsTrain, 
  metric = "Accuracy",
  method = "xgbTree",
  trControl = trainControl(method="cv", number = 2),
  na.action = 'na.pass',
  tuneGrid = expand.grid(
    #nrounds = seq(from = 500, to = 2000, by = 500),
    nrounds = 20,
    max_depth = 2,
    #max_depth = c(6, 8, 10),
    #eta = c(0.01, 0.02),  #Shrinkage
    eta = 0.02,  #Shrinkage
    gamma = 0,
    colsample_bytree = 0.5,
    #colsample_bytree = c(0.5, 0.75),
    min_child_weight = 1,
    subsample = 0.5
    #subsample = c(0.5, 0.75)
  )
)

#Tune hyperparameters using tunegrid 
model<-train(
  factor(bas1) ~ ., 
  data = AsTrain, 
  metric = "Accuracy",
  method = "xgbTree",
  trControl = trainControl(method="cv", number = 10),
  na.action = 'na.pass',
  tuneGrid = expand.grid(
    nrounds = seq(from = 1000, to = 1000, by = 100),
    max_depth = seq(from = 6, to = 14, by = 2),
    eta = seq(from = 0.005, to = 0.0125, by = 0.0025),  #Shrinkage
    gamma = 0,
    colsample_bytree = seq(from = 0.25, to = 0.75, by = 0.25),
    min_child_weight = 1,
    subsample = seq(from = 0.25, to = 0.75, by = 0.25)
  )
)

saveRDS(model, paste("./",date, "as_cv10_final_model_xgb.rds", sep=""))





Asdata <- All_Asdata[sample(1:nrow(All_Asdata)), ]
#Asdata<-as.data.frame(Asdata)

# get the numb 70/30 training test split
#split into training (70%) and testing set (30%)
sample_set<-sample(nrow(Asdata), round(nrow(Asdata)*.7), replace = F)
As_train = Asdata[sample_set,]
As_test = Asdata[-sample_set,]

#Complete cases
As_trainComp <- As_train[complete.cases(As_train[,c(3,9:93)]),]  #col 3, testing on As >10 ug/L category
As_testComp<- As_test[complete.cases(As_test[,c(3,9:93)]),] #col 3testing on As >10 ug/L category

#As_trainComp <- As_trainComp[,c(3,9:93)] #col 3, testing on As >10 ug/L category
#As_testComp<- As_testComp[,c(3,9:93)] #col 3testing on As >10 ug/L category

#define predictor and response variables in training set
train_x<-data.matrix(As_trainComp[, -c(1:8)])
train_y<-As_trainComp[,3]

#define predictor and response variables in testing set
test_x<-data.matrix(As_testComp[, -c(1:8)])
test_y<-As_testComp[,3]

#define final training and testing sets
#xgb_train = xgb.DMatrix(data = train_x, label = train_y)  #OK, this runs when the outcome variable is numeric
#xgb_test = xgb.DMatrix(data = test_x, label = test_y)

#define watchlist
#watchlist = list(train=xgb_train, test=xgb_test)

#fit XGBoost model and display training and testing data at each round
#model = xgb.train(data = xgb_train, max.depth = 4, watchlist=watchlist, nrounds = 70, objective = "binary:logistic")




model

model$resample %>%
  arrange(Resample)

#make prediction and take out bas1 from data set
predictions <- predict(model, newdata = As_testComp[9:93])

#calculates the confusion matrix
conf_matrix <- confusionMatrix(predictions, factor(test_y))

#extract sensitivity, specificity, and balanced accuracy
sensitivity <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
balanced_accuracy <- conf_matrix$byClass["Balanced Accuracy"]

#results
sensitivity
specificity
balanced_accuracy
conf_matrix
