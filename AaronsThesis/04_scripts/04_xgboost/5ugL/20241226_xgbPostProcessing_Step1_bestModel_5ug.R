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
#library("SHAPforxgboost")
library(data.table)
library(tidyverse) # general utility functions


rm(list=ls())

# set data and seed values
date<-Sys.Date()
set.seed(1234)  # Setting seed 

#setwd("/Users/hoover/Documents/GitHub/coPlateauWaterQuality/03_data/")
setwd("/Users/aaronnuanez/Documents/GitHub/coPlateauWaterQuality/03_data/")


#Arsenic_xgb<-readRDS("./XGB_rds/2024-12-08_ClassLTE10_cv10_xgb.rds")
Arsenic_xgb<-readRDS("./XGB_rds/2024-12-05_ClassLTE5_cv10_xgb.rds")
Arsenic_xgb

#Make SiteID the row name so we can drop that field
#rownames(train)<-train$SiteID

ddd<-Arsenic_xgb$results

#identify maximum accuracy and then calculate 1 Se model threshold
a<-max(ddd$Accuracy); print(a) #accuracy
b<-ddd$AccuracySD[ddd$Accuracy==a] #standard deviation of 'best' model
se<-b/sqrt(10); print(se) #1 standard error 
aSe<-a-se; print(aSe) #'best' model minus 1 SE

#Filter to movdels with accuracy within 1 SE
dd3 <- ddd %>%
  filter(Accuracy >= aSe)

#Now filter by variable order, eta, max depth, nrounds
shrink<-min(dd3$eta); print(shrink)
depth<-min(dd3$max_depth); print(depth)


dd4 <- dd3 %>%
  filter(eta < 0.0125) %>%
  filter(max_depth == depth)

#best, most simple model
print(dd4) #use these parameters to train model using full training set, post process step 2, pick the model with the smallest values for nround, and sampling


#write.csv(ddd, file="20241226_As5_xgbAs_tuningOutput.csv")
