library(caTools) 
library(randomForest)
library(caret)
library(tidyverse)

#Set working directory
setwd("/Users/hoover/Documents/GitHub/coPlateauWaterQuality/01_data")
#setwd("/Users/austinmartinez/Documents/GitHub/coPlateauWaterQuality/01_data")

#Clean up the workspace
rm(list=ls())

date<-Sys.Date()
set.seed(1234)  # Setting seed 

#Load data
Asdata = read.csv("./CoPlateau_As/20240723_randomForest_As_dataClean.csv",
                  na.strings = "NA") #Probably need to simplify the path so the script and data are in the same folder for the HPC

Asdata <- Asdata[complete.cases(Asdata), ]

#Subset to training set
AsTrain<-subset(Asdata, trainCat3==TRUE)


#Ensure As3Cat is a Factor (Categorical Variable)
AsTrain$As3Cat <- as.factor(AsTrain$As3Cat)

#Drop unused fields
AsTrain<-AsTrain[,-c(1:6,213:215, 217)]

# Fitting Random Forest to the train dataset 
#classifier_RF 
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

saveRDS(Arsenic_boost, paste("./",date, "cv10_final_model_brt.rds", sep=""))
