library(caTools) 
library(caret)
library(gbm)
library(xgboost) # for xgboost
library(tidyverse) # general utility functions
#library(smotefamily) #use to balance the training dataset

#setwd("/Users/hoover/Documents/GitHub/coPlateauWaterQuality/01_data")
setwd("C:/Users/jhoover/Documents/GitHub/coPlateauWaterQuality/01_data")
#setwd("C:/Users/austinmartinez/Documents/GitHub/coPlateauWaterQuality/01_data")

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
As_trainComp <- As_train[complete.cases(As_train[,c(3,9:93)]),]  #col 3, testing on As >10 ug/L category
As_testComp<- As_test[complete.cases(As_test[,c(3,9:93)]),] #col 3testing on As >10 ug/L category

#define predictor and response variables in training set
train_x<-data.matrix(As_trainComp[, -c(1:8)])
train_y<-As_trainComp[,3]

#define predictor and response variables in testing set
test_x<-data.matrix(As_testComp[, -c(1:8)])
test_y<-As_testComp[,3]

#Create DMatrix for xgb
dTrain <- xgb.DMatrix(data = train_x, label= train_y)
dTest <- xgb.DMatrix(data = test_x, label= test_y)


#Create a list of parameters
paramDF <- expand.grid(
  max_depth = seq(from = 8, to = 14, by = 2),
  eta = seq(from = 0.005, to = 0.0125, by = 0.0025),  #Shrinkage
  colsample_bytree = seq(from = 0.25, to = 0.75, by = 0.25),
  subsample = seq(from = 0.25, to = 0.75, by = 0.25),
  gamma = 0,
  min_child_weight = 1
)

#Turn into a list of lists
paramList <- lapply(split(paramDF, 1:nrow(paramDF)), as.list)

#Loop through the parameter list and use a text progress bar
bestResults <- tibble()
set.seed(1234)
pb <- txtProgressBar(style = 3) 
for(i in seq(length(paramList))) {
  rwCV <- xgb.cv(params = paramList[[i]], 
                 data = dTrain, 
                 nrounds = 500,
                 nfold = 5,
                 early_stopping_rounds = 10,
                 verbose = FALSE)
  bestResults <- bestResults %>% 
    bind_rows(rwCV$evaluation_log[rwCV$best_iteration])
  gc() # Free unused memory after each loop iteration
  setTxtProgressBar(pb, i/length(paramList))
}
close(pb) # done with the progress bar

etasearch <- bind_cols(paramDF, bestResults)
View(etasearch)

#Display the folds sorted by RMSE
rwCV$evaluation_log %>% arrange(test_rmse_mean)

#WRite the best iteration
rwCV$evaluation_log[rwCV$best_iteration]



#Run XGB with tuned parameters
set.seed(1234)
rwMod <- xgb.train(data = dTrain, verbose = 3,
                   watchlist = list(train = dTrain, test = dTest), 
                   nrounds = 1000,
                   max_depth = 10,
                   eta = 0.005,  #Shrinkage
                   colsample_bytree = 0.75,
                   subsample = 0.5,
                   gamma = 0,
                   min_child_weight = 1)

rwMod$evaluation_log %>% 
  pivot_longer(cols = c(train_rmse, test_rmse), names_to = "RMSE") %>% 
  ggplot(aes(x = iter, y = value, color = RMSE)) + geom_line()


##Run the model with a different code block, outcome should be the same
pb <- txtProgressBar(style = 3) 
model<-train(
  data = As_trainComp[,c(3,9:93)],
  factor(bas10) ~ .,
  metric = "Accuracy",
  method = "xgbTree",
  verbose = FALSE,
  trControl = trainControl(method="cv", number = 1),
  tuneGrid = expand.grid(
    nrounds = 1000,
    max_depth = 14,
    eta = 0.0025,  #Shrinkage
    gamma = 0,
    colsample_bytree = 0.75,
    min_child_weight = 1,
    subsample = 0.75
  )
)
close(pb) # done with the progress bar

#Prediction using test data
predictions <- predict(model, newdata = As_testComp[,c(3,9:93)])

##
# Calculate confusion matrix
conf_matrix <- confusionMatrix(predictions, factor(test_y))
#> # Extract sensitivity, specificity, and balanced accuracy
sensitivity <- conf_matrix$byClass["Sensitivity"]; sensitivity
specificity <- conf_matrix$byClass["Specificity"]; specificity
balanced_accuracy <- conf_matrix$byClass["Balanced Accuracy"]; balanced_accuracy
