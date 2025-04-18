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

#As_trainComp <- As_trainComp[,c(3,9:93)] #col 3, testing on As >10 ug/L category
#As_testComp<- As_testComp[,c(3,9:93)] #col 3testing on As >10 ug/L category

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
                 nrounds = seq(from = 500, to = 2000, by = 500),  #this didn't work, it ran for nrounds = 500
                 nfold = 3,
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








# set up the cross-validated hyper-parameter search
xgb_grid_1 = expand.grid(
  nrounds = seq(from = 500, to = 2000, by = 500),
  max_depth = seq(from = 8, to = 14, by = 2),
  eta = seq(from = 0.005, to = 0.0125, by = 0.0025),  #Shrinkage
  gamma = 0,
  colsample_bytree = seq(from = 0.25, to = 0.75, by = 0.25),
  min_child_weight = 1,
  subsample = seq(from = 0.25, to = 0.75, by = 0.25)
)

# pack the training control parameters
xgb_trcontrol_1 = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",                                                        # save losses across all models
  classProbs = TRUE,                                                           # set to TRUE for AUC to be computed
  summaryFunction = twoClassSummary,
  allowParallel = TRUE
)


# train the model for each parameter combination in the grid, 
#   using CV to evaluate
xgb_train_1 = xgb.train(
  data=dTrain,
  params = 
  objective = "binary:logistic"
)





model<-xgb.train(
  data=dTrain,
  seq(from = 500, to = 2000, by = 500),
  max.depth = 8,
  eta = 0.005,
  gamma = 0,
  colsample_bytree = 0.25,
  min_child_weight = 1,
  subsample = 0.25,
  nthread = 6,
  objective = "binary:logistic",
  verbose = 2
)


#This model took ~40 minutes to run on my laptop, can still tune a few other parameters, you could try increasing the number to 5 or 10 but that will add lots more time. Might be best to run on a desktop in the lab. 
model<-train(
  factor(bas10) ~ ., 
  data = dTrain, 
  metric = "Accuracy",
  method = "xgbTree",
  verbose = 2,
  trControl = trainControl(method="cv", number = 5),
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


dtest <- xgb.DMatrix(data = test_data, label= test_labels)
#i get this error everytime
# > dtest <- xgb.DMatrix(data = test_data, label= test_labels)

model<-train(
  factor(bas10) ~ ., 
  data = As_trainComp, 
  metric = "Accuracy",
  method = "xgbTree",
  verbose = 1,
  trControl = trainControl(method="cv", number = 1),
  tuneGrid = expand.grid(
    nrounds = seq(from = 500, to = 500, by = 500),
    max_depth = seq(from = 8, to = 8, by = 2),
    eta = seq(from = 0.005, to = 0.01, by = 0.0025),  #Shrinkage
    gamma = 0,
    colsample_bytree = seq(from = 0.25, to = 0.75, by = 0.25),
    min_child_weight = 1,
    subsample = seq(from = 0.25, to = 0.75, by = 0.25)
  )
)

model

model$resample %>%
  arrange(Resample)

##Take a look at this reference to read about SHAP plots: https://christophm.github.io/interpretable-ml-book/shap.html
##Try implementing some of this code: https://github.com/liuyanguu/SHAPforxgboost try the summary plot and dependence plots
#Check out what they did in this USGS paper: https://pubs.acs.org/doi/10.1021/acs.est.3c03315

#Rerun the model and tune for >1 ug/L, >5 ug/L and >10 ug/L (the current model)`




##I didn't use this code below
#turning all 1-0(true and false) to booleans
# List of non-numeric variables
non_numeric_vars <- c(
  "rt_carb", "rt_clast_c", "rt_clast_f", "rt_clast_u", "rt_meta", "rt_plut_qtz",
  "D3", "De", "DSe", "Kg", "O2", "Oe", "PP4", "Pzg1", "Pzg2", "Q", "Qv", "S2", "Se",
  "Te2", "Tm", "Tmc", "Toc", "Tp", "Tpc", "Tpv", "Tr", "Txc", "uK2", "uK3", "uK3b",
  "uK4", "uPz", "Wgn", "uc_12", "uc_221", "uc_312", "uc_321", "uc_322", "uc_331",
  "uc_421", "uc_422", "uc_431", "uc_451", "uc_620", "uc_811", "uc_812", "uc_821",
  "uc_822", "uc_910", "uc_920", "uc_940", "uc_960", "uc_970", "uc_999", "na_10.1",
  "na_11.1", "na_12.1", "na_13.1", "na_6.2", "na_7.1", "na_8.1", "na_8.2", "na_9.2",
  "na_9.3", "na_9.4", "na_9.5", "na_9.6"
)

#By using ~ . == 1, it ensures that each column's values are evaluated such that 1 becomes TRUE and 0 becomes FALSE
AsLabels <- Asdata %>%
  select(all_of(non_numeric_vars)) %>%
  mutate(across(everything(), ~ . == 1))

# Removes rows 1-8(unessecary data) and removes all nonnumeric vars because they aren't booleans
clean_Asdata <- Asdata %>%
  select(-c(1:8, all_of(non_numeric_vars)))

#combine AsLabels(booleaned nonnumeric vars) and the clean_Asdata set
FullcleanAs <- cbind(AsLabels, clean_Asdata)

# Ensure the result is a data frame
FullcleanAs <- as.data.frame(FullcleanAs)

# get the numb 70/30 training test split
#split into training (70%) and testing set (30%)
sample_set<-sample(nrow(FullcleanAs), round(nrow(FullcleanAs)*.7), replace = F)
As_train = FullcleanAs[sample_set,]
As_test = FullcleanAs[-sample_set,]

# make it a metrix
#As_matrix <- FullcleanAs %>%
#  data.matrix()


# As3Cat is the target variable
train_labels <- All_Asdata$As3Cat 
test_labels <- All_Asdata$As3Cat




#FEEL FREE TO COMMENT OUT. This model dosen't work wether or not its used)
# I'm making As3Cat into a true or false senerio.(they did this in the tutotial)
# C3=true(over the amount of safe As conc) C2,C1=False(under the Safe As conc)
# Create a binary label indicating if the concentration is in C3
#training
binary_labels <- ifelse(test_labels == "C3", 1, 0)
# Verify the transformation
table(binary_labels)
# Optionally, update test_labels to binary_labels if you want to use it directly
test_labels <- binary_labels
#testing
binary_labels <- ifelse(test_labels == "C3", 1, 0)
# Verify the transformation
table(binary_labels)
# Optionally, update test_labels to binary_labels if you want to use it directly
test_labels <- binary_labels




# training data
train_data <- As_matrix[1:numberOfTrainingSamples,]
train_labels <- train_labels[1:numberOfTrainingSamples]

# testing data
test_data <- As_matrix[-(1:numberOfTrainingSamples),]
test_labels <- test_labels[-(1:numberOfTrainingSamples)]

#take out NAs
train_data <- train_data[complete.cases(train_data)]
test_data <- test_data[complete.cases(test_data)]
train_labels <- train_labels[complete.cases(train_labels)]
test_labels <- test_labels[complete.cases(test_labels)]

# putting the testing and training data into two seperates Dmatrixs
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)
#i get this error everytime
# > dtest <- xgb.DMatrix(data = test_data, label= test_labels)
#Error in xgb.DMatrix(data = test_data, label = test_labels) : 
  #xgb.DMatrix does not support construction from double
