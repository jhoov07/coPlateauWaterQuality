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
Asdata = read.csv("./CoPlateau_As/20240723_Cleaned_As_GIS_Filtered.csv", 
                  na.strings = "NULL") #Probably need to simplify the path so the script and data are in the same folder for the HPC

Asdata <- Asdata %>%
  mutate(
    As3Cat = case_when(
      ResultMeasureValue > 10 ~ "C3",
      ResultMeasureValue > 5 ~ "C2",
      ResultMeasureValue >= 1 ~ "C1",
      TRUE ~ "C1"  # Values below 1 will also be C1
    )
  )

# Drop rows with NA values in predictor varialble fields and outcome for test data
Asdata2 <- Asdata[complete.cases(Asdata[, 7:214]), ]

# get the numb 70/30 training test split
#split into training (70%) and testing set (30%), keep traing set balances with overall distribution
sample_set<-sample.split(Asdata2$As3Cat, SplitRatio = 0.7)

Asdata2 <- Asdata2 %>%
  mutate(
    trainCat3 = ifelse(sample_set == TRUE, 1, 0)
  )

write.csv(Asdata2, "./CoPlateau_As/20240723_randomForest_As_dataClean.csv")
