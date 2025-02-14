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
a<-list("pH", "Fe", "prism30yr", "A_Calcite", "DepthToGW", "A_Kaolinit", "C_Se", "C_Sb", "A_Quartz", 
        "Top5_Ca", "A_Tot_Flds", "C_Hematite", "C_Tot_14A", "A_Hg", "A_Tl", "A_C_Tot", "C_Cr", "C_Kaolinit", 
        "Top5_As", "Top5_Ba")

#define predictor and response variables in training set, As= 5 ug/L, keep variables defined above
train_x = data.matrix(train[, c(3,2,5,27,108,38,88,87,47,11,60,99,106,36,56,26,71,102,8,9)])
train_y = train[,160]

#define predictor and response variables in testing set
test_x = data.matrix(test[, c(3,2,5,27,108,38,88,87,47,11,60,99,106,36,56,26,71,102,8,9)])
test_y = test[,160]

#define final training and testing sets
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

#define watchlist
watchlist = list(train=xgb_train, test=xgb_test)

#Set parameters from all the tuning, steps 2 and 3
params = list(alpha = 0,
              lambda = 1,
              gamma = 2,
              max_delta_step = 0,
              eta = 0.01,
              max_depth = 4,
              subsample = 0.50,
              colsample_bytree = 0.75,
              min_child_weight = 1,
              booster = "gbtree")

#Fully tuned model
model = xgboost(data = xgb_train, params = params,
                   nrounds = 1000, objective = "binary:logistic",
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
#for spatial data
library(raster)
library(sp)
library(terra)

#wd <- ("/Users/hoover/desktop/")
wd <- ("/Users/aaronnuanez/desktop/")


rasterlist2 <-  list.files(paste0(wd,"spatialPredFormattedTifs"), full.names=TRUE, pattern=".tif$")
rasterlist2

#d<-"/Users/hoover/desktop/spatialPredFormattedTifs/"
d<-"/Users/aaronnuanez/desktop/spatialPredFormattedTifs/"


#Load each raster to check extent and crop as needed
A_C_Tot<-raster(paste(d,"A_C_Tot.tif", sep=""))
A_Calcite<-raster(paste(d,"A_Calcite.tif", sep=""))
A_Hg<-raster(paste(d,"A_Hg.tif", sep=""))
A_Kaolinit<-raster(paste(d,"A_Kaolinit.tif", sep=""))
A_Quartz<-raster(paste(d,"A_Quartz.tif", sep=""))
A_Tl<-raster(paste(d,"A_Tl.tif", sep=""))
A_Tot_Flds<-raster(paste(d,"A_Tot_Flds.tif", sep=""))

C_Cr<-raster(paste(d,"C_Cr.tif", sep=""))
C_Hematite<-raster(paste(d,"C_Hematite.tif", sep=""))
C_Kaolinit<-raster(paste(d,"C_Kaolinit.tif", sep=""))
C_Sb<-raster(paste(d,"C_Sb.tif", sep=""))
C_Se<-raster(paste(d,"C_Se.tif", sep=""))
C_Tot_14A<-raster(paste(d,"C_Tot_14A.tif", sep=""))

DepthToGW<-raster(paste(d,"DepthToGW.tif", sep=""))
Fe<-raster(paste(d,"Fe.tif", sep=""))
pH<-raster(paste(d,"pH.tif", sep=""))
prism30yr<-raster(paste(d,"prism30yr.tif", sep=""))

Top5_As<-raster(paste(d,"Top5_As.tif", sep=""))
Top5_Ba<-raster(paste(d,"Top5_Ba.tif", sep=""))
Top5_Ca<-raster(paste(d,"Top5_Ca.tif", sep=""))

#Rename fields to match model names
A_C_Tot@data@names<-"A_C_Tot"
A_Calcite@data@names<-"A_Calcite"
A_Hg@data@names<-"A_Hg"
A_Kaolinit@data@names<-"A_Kaolinit"
A_Quartz@data@names<-"A_Quartz"
A_Tl@data@names<-"A_Tl"
A_Tot_Flds@data@names<-"A_Tot_Flds"

C_Cr@data@names<-"C_Cr"
C_Hematite@data@names<-"C_Hematite"
C_Kaolinit@data@names<-"C_Kaolinit"
C_Sb@data@names<-"C_Sb"
C_Se@data@names<-"C_Se"
C_Tot_14A@data@names<-"C_Tot_14A"

DepthToGW@data@names<-"DepthToGW"
Fe@data@names<-"Fe"
pH@data@names<-"pH"
prism30yr@data@names<-"prism30yr"

Top5_As@data@names<-"Top5_As"
Top5_Ba@data@names<-"Top5_Ba"
Top5_Ca@data@names<-"Top5_Ca"

#[1] "pH"         "Fe"         "prism30yr"  "A_Calcite"  "DepthToGW"  "A_Kaolinit" "C_Se"       "C_Sb"      
#[9] "A_Quartz"   "Top5_Ca"    "A_Tot_Flds" "C_Hematite" "C_Tot_14A"  "A_Hg"       "A_Tl"       "A_C_Tot"   
#[17] "C_Cr"       "C_Kaolinit" "Top5_As"    "Top5_Ba"  

# create raster stack and convert to a maxtrix so it works with predict function for XGB
rstack1 <- stack(pH, Fe, prism30yr, A_Calcite, DepthToGW, A_Kaolinit, C_Se, C_Sb, A_Quartz,
                 Top5_Ca, A_Tot_Flds, C_Hematite, C_Tot_14A, A_Hg, A_Tl, A_C_Tot,
                 C_Cr, C_Kaolinit, Top5_As, Top5_Ba)
rstack2<-rasterToPoints(rstack1)

#Make spatial prediction
spatialPred <- as.data.frame(predict (model, rstack2[,-c(1,2)]))
colnames(spatialPred)[1]<-"AsPredict"

rstack3<-as.data.frame(rstack2)
rstack3$AsPred<-spatialPred$AsPredict

#Convert to raster
#crs<-paste(Fe@srs)
r<-rasterFromXYZ(rstack3[,c(1,2,23)], res=c(500,500))

#Make a plot and write to file
plot(r)

#Write to file
writeRaster(r, "/Users/aaronnuanez/Desktop/20250214_XGB_probAs5ugL", format='GTiff')

