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
train <- Asdata[Asdata$trainClassLTE5_splt == TRUE, ] 
test <- Asdata[Asdata$trainClassLTE5_splt == FALSE, ] 

#Make SiteID the row name so we can drop that field
rownames(train)<-train$SiteID
rownames(test)<-test$SiteID

#Make a list of the fewest number of variables with the highest overall prediction accuracy
#("pH", "Fe", "prism30yr", "A_Calcite", "DepthToGW", "A_Kaolinit", "C_Se", "C_Sb", "A_Quartz", "Top5_Ca", "A_Tot_Flds", "C_Hematite", "C_Tot_14A", "A_Hg", "A_Tl", "A_C_Tot", "C_Cr", "C_Kaolinit", "Top5_As", "Top5_Ba", "C_U")
#(3,2,5,27,108,38,88,87,47,11,60,99,106,36,56,26,71,102,8,9,93)

#highest accuracy is 0.755 using 20 variables with the highest gain values - from the csv output from step 3
a<-list("pH", "Fe", "prism30yr", "A_Calcite", "DepthToGW", "A_Kaolinit", "C_Se", "C_Sb", "A_Quartz")

#For 21 Variables and 0.757 Accuracy, add C_U and 93

#define predictor and response variables in training set, As= 5 ug/L, keep variables defined above
train_x = data.matrix(train[, c(3,2,5,27,108,38,88,87,47)])
#train_x = data.matrix(train[, -c(1, 4, 109:112, 157:168)])
train_y = train[,160]

#define predictor and response variables in testing set
test_x = data.matrix(test[, c(3,2,5,27,108,38,88,87,47)])
test_y = test[, 160]

#define final training and testing sets
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

#define watchlist
watchlist = list(train=xgb_train, test=xgb_test)

#Use fully tuned hyperparameters from steps 1 and 2
dfAc<-data.frame()
params = list(alpha = 0,
              lambda = 1,
              gamma = 2,
              max_delta_step = 0,
              eta = 0.01,
              max_depth = 4,
              subsample = 0.5,
              colsample_bytree = 0.75,
              min_child_weight = 1,
              booster = "gbtree")

##XGB Train
for(data in 1:10){
  model = xgb.train(data = xgb_train, params = params,
                    watchlist = watchlist,
                    nrounds = 1000, objective = "binary:logistic",
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

wd <- ("/Users/hoover/desktop/")

# Open your reference Raster
#Reference_Raster <- list.files(paste0(wd,"15VariablesforXGB10ugL"), full.names=TRUE, pattern = "Fe_500m.tif$")
#Reference_Raster <- raster(Reference_Raster)

# Open the other raster
#Raster <- list.files(paste0(wd,"15VariablesforXGB10ugL"), full.names=TRUE, pattern=".tif$")
#Raster <- raster(Raster)

## ----list-files-tif--------------------------------------------------
rasterlist2 <-  list.files(paste0(wd,"15VariablesforXGB10ugL"), full.names=TRUE, pattern=".tif$")
rasterlist2

#Load each raster to check extent and crop as needed
A_Aragon<-raster("/Users/hoover/desktop/15VariablesforXGB10ugL/A_Aragon_500m.tif" )
A_Calcite<-raster("/Users/hoover/desktop/15VariablesforXGB10ugL/A_Calcite_500m.tif" )
A_Cs<-raster("/Users/hoover/desktop/15VariablesforXGB10ugL/A_Cs_500m.tif" )
A_Tot_14A<-raster("/Users/hoover/desktop/15VariablesforXGB10ugL/A_Tot_14A.tif" )

C_Amorph<-raster("/Users/hoover/desktop/15VariablesforXGB10ugL/C_Amorph.tif")
C_Analcime<-raster("/Users/hoover/desktop/15VariablesforXGB10ugL/C_Analcime.tif")
C_Cr<-raster("/Users/hoover/desktop/15VariablesforXGB10ugL/C_Cr_500m.tif")
C_Hematite<-raster("/Users/hoover/desktop/15VariablesforXGB10ugL/C_Hematite_500m.tif")
C_Mo<-raster("/Users/hoover/desktop/15VariablesforXGB10ugL/C_Mo.tif")

Fe<-raster("/Users/hoover/desktop/15VariablesforXGB10ugL/Fe_500m.tif" )
pH<-raster("/Users/hoover/desktop/15VariablesforXGB10ugL/pH_500m.tif" )
prism30yr<-raster("/Users/hoover/desktop/15VariablesforXGB10ugL/prism30yr_500m.tif" )
Top5_Ca<-raster("/Users/hoover/desktop/15VariablesforXGB10ugL/Top5_Ca.tif")
Top5_S<-raster("/Users/hoover/desktop/15VariablesforXGB10ugL/Top5_S_500m.tif")

#Resample
A_Aragon<-resample(A_Aragon, Fe, method = "ngb")
A_Calcite<-resample(A_Calcite, Fe, method = "ngb")
A_Cs<-resample(A_Cs, Fe, method = "ngb")
A_Tot_14A<-resample(A_Tot_14A, Fe, method = "ngb")

C_Amorph<-resample(C_Amorph, Fe, method = "ngb")
C_Analcime<-resample(C_Analcime, Fe, method = "ngb")
C_Cr<-resample(C_Cr, Fe, method = "ngb")
C_Hematite<-resample(C_Hematite, Fe, method = "ngb")
C_Mo<-resample(C_Mo, Fe, method = "ngb")

prism30yr <- resample(prism30yr, Fe, method = "ngb")
Top5_Ca <- resample(Top5_Ca, Fe, method = "ngb")
Top5_S <- resample(Top5_S, Fe, method = "ngb")

# create raster stack
rstack1 <- stack(A_Aragon,A_Calcite,A_Cs,A_Tot_14A,
                 C_Amorph,C_Analcime,C_Cr,C_Hematite,C_Mo,
                 Fe,pH,prism30yr,Top5_Ca,Top5_S)

