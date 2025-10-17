# This script will take execute random forest for an input dataset
# such database will be outputted in wide or narrow format
# Author: Mery Touceda-Suarez (mtoucedasuarez@arizona.edu) and Joe Hoover (jhoover@arizona.edu)
# Date: July 2024

# USAGE: 
# Rscript [PATH-TO-SRIPT]/create_db_from_csvs.R [IN-PATH] [OUT-PATH] [DB-NAME]
# Example:
# Rscript Desktop/step2_rf_as_modelProcessing_forHPC.R Downloads/data Documents/waterproject/database indigenousWaterQuality


# SETUP
#Load and install packages

#install.packages("caTools")
#install.packages("randomForest")
#install.packages("caret")
#install.packages("tidyverse")

# load libraries

library(caTools)
library(randomForest)
library(caret)
library(tidyverse)

# load arguments
args<-commandArgs(TRUE)

## Input files and directories
in_path=args[1]
out_pathDir=args[2]
#out_name=args[3]

# set data and seed values
date<-Sys.Date()
set.seed(1234)  # Setting seed 

#Load data
Asdata = read.csv(in_path, na.strings = "NA")

#Confirm complete cases for input data
Asdata <- Asdata[complete.cases(Asdata), ]

#Subset to training set
AsTrain<-subset(Asdata, trainCat3==TRUE)

#Ensure As3Cat is a Factor (Categorical Variable)
AsTrain$As3Cat <- as.factor(AsTrain$As3Cat)

#Drop unused fields
AsTrain<-AsTrain[,-c(1:6,213:215, 217)]

#Set a tune grid
n<-ncol(AsTrain)-1
tunegrid <- expand.grid(mtry = 1:n)

# Fitting Boosted Regression Tree to the train dataset 
  Arsenic_boost <- train(
  As3Cat ~ .,  # Specify the target variable as As3Cat
  data = AsTrain,  
  method = "gbm", 
  trControl = trainControl(
    method = "cv",
    number = 10,    #Use 10 fold for real runs
    verboseIter = TRUE  # Enable verbose output for troubleshooting
  ),
  tuneGrid = expand.grid(
    "n.trees" = seq(from = 100, to = 500, by = 10),  #from USGS paper, might want to scale down for our work here
    "interaction.depth" = seq(from = 2, to = 16, by = 2),  #adapted from USGS paper, might want to scale down for our work here
    "shrinkage" = seq(from = 0.004, to = 0.012, by = 0.002),  #adapted from USGS paper, might want to scale down for our work here
    "n.minobsinnode" = 8) #from USGS paper, might want to scale down for our work here
)

#Save model as an object file
rds_out_name <- paste(date, "as_brt.rds", sep = "_")
rds_out_path <- paste(out_pathDir, rds_out_name, sep="/")
saveRDS(Arsenic_boost, rds_out_path)