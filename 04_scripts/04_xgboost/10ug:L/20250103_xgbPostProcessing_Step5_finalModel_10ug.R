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
library(cutpointr)
#library(tidyverse)

#for spatial data
library(raster)
library(sp)


rm(list=ls())

# set data and seed values
date<-Sys.Date()
set.seed(1234)  # Setting seed 

setwd("/Users/hoover/Documents/GitHub/coPlateauWaterQuality/03_data/")
#setwd("/Users/aaronnuanez/Documents/GitHub/coPlateauWaterQuality/03_data/")

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
a<-list("pH", "prism30yr", "A_Cs", "A_Aragon", "C_Hematite", "Fe", "Top5_S", "C_Cr", "A_Calcite", 
        "DepthToGW", "C_Mo", "Top5_Ca", "A_Tot_14A", "C_Amorph", "C_Analcime")

#define predictor and response variables in training set, As= 10 ug/L, keep variables defined above
train_x = data.matrix(train[, c(3, 5, 29, 25, 99, 2, 17, 71, 27, 108, 80, 11, 58, 65, 66)])
train_y = train[,161]

#define predictor and response variables in testing set
test_x = data.matrix(test[, c(3, 5, 29, 25, 99, 2, 17, 71, 27, 108, 80, 11, 58, 65, 66)])
test_y = test[,161]

#define final training and testing sets
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

#define watchlist
watchlist = list(train=xgb_train, test=xgb_test)

#Set parameters from all the tuning, steps 2 and 3
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

#Fully tuned model
model = xgboost(data = xgb_train, params = params,
                   nrounds = 750, objective = "binary:logistic",
                   eval_metric = "error", verbose = 1,
                   print_every_n = 100)
  

#write.csv(dfAc, file="20241223_modelTuning_primaryHyperparameters_alpha2Lambda5.csv")

#Testing Data
xgbpred <- predict (model, xgb_test)
xgbpred2 <- ifelse (xgbpred > 0.5,1,0)
confusionMatrix (factor(xgbpred2), factor(test_y)) #keep this for reporting

#Adjust the "true" threshold using Youden value
#For a figure
y_predJoin<-data.frame(cbind(test_y, xgbpred))#change field to match outcome modeled, this applies to LT10

#rename fields for ease of use
colnames(y_predJoin)[1]<-"Obsclass"
colnames(y_predJoin)[2]<-"Predexceed"

#Use cutpoint to identify threshold for As 'detection' balancing sensitivity and specificity using Youden metric
cp <- cutpointr(y_predJoin, Predexceed, Obsclass, 
                method = maximize_metric, metric = youden, pot_class = 1)
summary(cp) #make note of the cutpoint value for comparision with lines 91-93 above
plot(cp)

#Extract ROC Curve data for plotting
a<-as.data.frame(cp$roc_curve)
a$sens<-a$tp/(a$tp+a$fn) #sensitivity
a$spec<-a$tn/(a$tn+a$fp) #specificity
a$j<-(a$tp/(a$tp+a$fn))+(a$tn/(a$tn+a$fp))-1 #j-index, also called Youden value

##Make a plot like USGS PFAS paper S8
library(tidyverse)
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


#Load raster files for prediction model
wd <- ("/Users/hoover/desktop/")
rasterlist2 <-  list.files(paste0(wd,"spatialPredFormattedTifs"), full.names=TRUE, pattern=".tif$")
rasterlist2

d<-"/Users/hoover/desktop/spatialPredFormattedTifs/"

#library(terra)

#Load each raster to check extent and crop as needed
A_Aragon<-raster(paste(d, "A_Aragon.tif", sep=""))
A_Calcite<-raster(paste(d,"A_Calcite.tif", sep=""))
A_Cs<-raster(paste(d,"A_Cs.tif", sep=""))
A_Tot_14A<-raster(paste(d,"A_Tot_14A.tif", sep=""))

C_Amorph<-raster(paste(d,"C_Amorph.tif", sep=""))
C_Analcime<-raster(paste(d,"C_Analcime.tif", sep=""))
C_Cr<-raster(paste(d,"C_Cr.tif", sep=""))
C_Hematite<-raster(paste(d,"C_Hematite.tif", sep=""))
C_Mo<-raster(paste(d,"C_Mo.tif", sep=""))

Fe<-raster(paste(d,"Fe.tif", sep=""))
pH<-raster(paste(d,"pH.tif", sep=""))
prism30yr<-raster(paste(d,"prism30yr.tif", sep=""))
Top5_Ca<-raster(paste(d,"Top5_Ca.tif", sep=""))
Top5_S<-raster(paste(d,"Top5_S.tif", sep=""))

DepthToGW<-raster(paste(d,"DepthToGW.tif", sep=""))

#Change names so they match the XGB model
A_Aragon@data@names<-"A_Aragon"
A_Calcite@data@names<-"A_Calcite"
A_Cs@data@names<-"A_Cs"
A_Tot_14A@data@names<-"A_Tot_14A"

C_Amorph@data@names<-"C_Amorph"
C_Analcime@data@names<-"C_Analcime"
C_Cr@data@names<-"C_Cr"
C_Hematite@data@names<-"C_Hematite"
C_Mo@data@names<-"C_Mo"

Fe@data@names<-"Fe"
pH@data@names<-"pH"
prism30yr@data@names<-"prism30yr"
Top5_Ca@data@names<-"Top5_Ca"
Top5_S@data@names<-"Top5_S"

DepthToGW@data@names<-"DepthToGW"


# create raster stack and convert to a maxtrix so it works with predict function for XGB
rstack1 <- stack(pH, prism30yr, A_Cs, A_Aragon, C_Hematite, Fe, Top5_S, 
                           C_Cr, A_Calcite, DepthToGW, C_Mo, Top5_Ca, A_Tot_14A,
                           C_Amorph,C_Analcime)
rstack2<-rasterToPoints(rstack1)

#Make spatial prediction
spatialPred <- as.data.frame(predict (model, rstack2[,-c(1,2)]))
colnames(spatialPred)[1]<-"AsPredict"

rstack3<-as.data.frame(rstack2)
rstack3$AsPred<-spatialPred$AsPredict

#Convert to raster
#crs<-paste(Fe@srs)
r<-rasterFromXYZ(rstack3[,c(1,2,18)], res=c(500,500))

#Make a plot and write to file
plot(r)

#Write to file
writeRaster(r, "20250130_probAs10ugL", format='GTiff')




spatialPred2 <- predict (model, rstack2[,-c(1,2)], )

#Assign output probability to rstack2




plot(Fe)
str(Fe)

