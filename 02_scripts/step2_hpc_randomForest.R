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

#Set a tune grid
n<-ncol(AsTrain)-1
#tunegrid <- expand.grid(mtry = 1:n) #Change to 1:84 if testing for real, 1:3 was used for model development
tunegrid <- expand.grid(mtry = 1:15) #Change to 1:84 if testing for real, 1:3 was used for model development

#Turn into a list of lists
paramList <- lapply(split(tunegrid, 1:nrow(tunegrid)), as.list)
#

# Fitting Random Forest to the train dataset 
#classifier_RF 

for(i in seq(length(paramList))) {
  classifier_RF<-train(
    data = AsTrain, 
    factor(As3Cat) ~ .,
    metric = "Accuracy",
    method = "rf",
    trControl = trainControl(method="cv", number = 3),    #change number = 10 if doing for real
    tuneGrid  = tunegrid,
    ntree = 500,
    verboseIter = TRUE)  # Enable verbose output for troubleshooting
}

saveRDS(classifier_RF, paste("./",date, "mtry15_cv3_final_model_rf.rds", sep=""))
