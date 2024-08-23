setwd("/Users/hoover/desktop/data")

#Load libraries
#library(tidyverse)
library(dplyr)
library(reshape2)

#Clean up the workspace
rm(list=ls())

#Load data from WQP
fe <- read.csv("iron.csv", na.strings = "NULL")
ca <- read.csv("Ca.csv" , na.strings = "NULL")
alkalinity <- read.csv("Alkalinity.csv" , na.strings = "NULL")
ph <- read.csv("pH.csv" , na.strings = "NULL")

#Load existing WQP data file that's already been cleaned
#data <-read.csv()

#data2 <- data %>%
#  filter(analyte == "As" |
  
#Filter fields in new WQP data
fe <- fe %>%
  select(SiteID, ActivityStartDate, CharacteristicName, ResultSampleFractionText, ResultMeasureValue, ResultMeasureMeasureUnitCode) %>%
  filter(ResultSampleFractionText == "Dissolved")

ca <- ca %>%
  select(SiteID, ActivityStartDate, CharacteristicName, ResultSampleFractionText, ResultMeasureValue, ResultMeasureMeasureUnitCode) %>%
  filter(ResultSampleFractionText == "Dissolved")

# Converted ResultMeasureValue from mg/L to ug/L for rows where ResultMeasureMeasureUnitCode is mg/L
#Fe
summary(factor(fe$ResultMeasureMeasureUnitCode)) #Check to see what units are noted in the field
mgL_indices <- which(fe$ResultMeasureMeasureUnitCode == "mg/l" | fe$ResultMeasureMeasureUnitCode == "mg/L") #Create an idex with records that we need to convert
fe$ResultMeasureValue[mgL_indices] <- fe$ResultMeasureValue[mgL_indices] * 1000 #Convert to ug/L to match NN Wells analyte data
fe$ResultMeasureMeasureUnitCode <- "ug/L"   # made all units ug/L

# Ca
summary(factor(ca$ResultMeasureMeasureUnitCode)) #Check to see what units are noted in the field
ugL_indices <- which(ca$ResultMeasureMeasureUnitCode == "ug/l" | ca$ResultMeasureMeasureUnitCode == "ug/L") #Create an idex with records that we need to convert
ca$ResultMeasureValue[ugL_indices] <- ca$ResultMeasureValue[ugL_indices]/1000 #Convert to ug/L to match NN Wells analyte data
ca$ResultMeasureMeasureUnitCode <- "mg/L"   # made all units mg/L
 
#Add in other analytes, pH and alkalinity
 
#Merge data frames
dataM<-rbind(fe, ca)

#Clear up field types
dataM$SiteID<-factor(dataM$SiteID)
dataM$CharacteristicName<-factor(dataM$CharacteristicName)
#Wide format
dataW<-dcast(dataM, SiteID~CharacteristicName, value.var="ResultMeasureValue", mean)
dataM$ResultMeasureValue

#I need to see where this code takes us and if we need any additional processing steps before combining with the other WQP data
  
#write.csv(combined_data, file = "~/Desktop/#combined_analytes_nnwells.csv", row.names = FALSE)
