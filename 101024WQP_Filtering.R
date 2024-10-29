#Merge WQP data tables

#Load libraries
library(reshape2)
library(dplyr)
library(tidyr)

#Set working directory
setwd("/Volumes/HooverShare/Shared_Group_Data/20_projects/06_coPlateau_Rework/")

#Clean up the workspace
rm(list=ls())

#Load csv files
a<-read.csv("./02_Data/Raw_Data/WQP/00_archive/AZ_Uranium.csv", na.strings = "NULL")
b<-read.csv("./02_Data/Raw_Data/WQP/00_archive/NM_Uranium.csv", na.strings = "NULL")
c<-read.csv("./02_Data/Raw_Data/WQP/00_archive/Ca.csv", na.strings = "NULL")
d<-read.csv("./02_Data/Raw_Data/WQP/00_archive/Iron.csv", na.strings = "NULL")
e<-read.csv("./02_Data/Raw_Data/WQP/00_archive/ph.csv", na.strings = "NULL")
f<-read.csv("./02_Data/Raw_Data/WQP/00_archive/alkalinity.csv", na.strings = "NULL")
g<-read.csv("./02_Data/Raw_Data/WQP/00_archive/CO_Uranium.csv", na.strings = "NULL")
h<-read.csv("./02_Data/Raw_Data/WQP/00_archive/UT_Uranium.csv", na.strings = "NULL")

#Process the files
#Merge U files into a single data frame (done)
u<-rbind(a,b,g,h)

#Process uranium data (done)
u2 <- u %>%
  drop_na(ResultMeasureMeasureUnitCode) %>%
  filter(ResultSampleFractionText == "Dissolved") %>%
  filter(ResultMeasureMeasureUnitCode != "ratio") %>%
  filter(CharacteristicName == "U")
  
  
summary(factor(u2$ResultMeasureMeasureUnitCode)) #Check to see what units are noted in the field
mgL_indices <- which(u2$ResultMeasureMeasureUnitCode == "pCi/L") #Create an index with records that we need to convert
u2$ResultMeasureValue[mgL_indices] <- u2$ResultMeasureValue[mgL_indices] * 0.67 #Convert to pCi/L to ug/L match NN Wells analyte data
u2$ResultMeasureMeasureUnitCode <- "ug/L"   # made all units mg/L


#Calcium: keep mg/L (done)
c2 <- c %>%
  drop_na(ResultMeasureMeasureUnitCode) %>%
  filter(ResultSampleFractionText == "Dissolved")
  
summary(factor(c2$ResultMeasureMeasureUnitCode)) #Check to see what units are noted in the field
mgL_indices <- which(c2$ResultMeasureMeasureUnitCode == "ug/l" | c2$ResultMeasureMeasureUnitCode == "ug/L") #Create an index with records that we need to convert
c2$ResultMeasureValue[mgL_indices] <- c2$ResultMeasureValue[mgL_indices] / 1000 #Convert to mg/L to match NN Wells analyte data
c2$ResultMeasureMeasureUnitCode <- "mg/L"   # made all units mg/L

#Iron: unit conversions (done)
d2 <- d %>%
  drop_na(ResultMeasureMeasureUnitCode) %>%
  filter(ResultSampleFractionText == "Dissolved") %>%
  filter(CharacteristicName == "Fe") #remove Ferric ion, Ferrous ion, and Iron-59

summary(factor(d2$ResultMeasureMeasureUnitCode)) #Check to see what units are noted in the field
mgL_indices <- which(d2$ResultMeasureMeasureUnitCode == "mg/l" | d2$ResultMeasureMeasureUnitCode == "mg/L") #Create an index with records that we need to convert
d2$ResultMeasureValue[mgL_indices] <- d2$ResultMeasureValue[mgL_indices] * 1000 #Convert to ug/L to match NN Wells analyte data
d2$ResultMeasureMeasureUnitCode <- "ug/L"   # made all units ug/L

#pH - check that all measurements are in standard units (done)
e2 <- e %>%
  drop_na(ResultMeasureMeasureUnitCode) %>%
  filter(ResultSampleFractionText == "Total")

#summary(factor(e2$ResultMeasureMeasureUnitCode))

#Alkalinity: convert ug/L to mg/L in ResultMeasureMeasureUnitCode (done)
f2 <- f %>%
  drop_na(ResultMeasureMeasureUnitCode) %>%   #Remove rows with NA's using drop_na()
  filter(ResultSampleFractionText == "Total") %>% #filter to total results since we want to use raw water samples
  filter(CharacteristicName == "Alkalinity")


summary(factor(f2$ResultMeasureMeasureUnitCode)) #Check to see what units are noted in the field
mgL_indices <- which(f2$ResultMeasureMeasureUnitCode == "ug/l") #Create an index with records that we need to convert
f2$ResultMeasureValue[mgL_indices] <- f2$ResultMeasureValue[mgL_indices] / 1000 #Convert to ug/L to match NN Wells analyte data
f2$ResultMeasureMeasureUnitCode <- "mg/L"   # made all units mg/L

#Merge files
cdef<-rbind(c2,d2,e2,f2, u2)

cdef2 <- cdef %>%
  filter(StateCode == 4 & (CountyCode == 1 | CountyCode == 5 | CountyCode == 7 | CountyCode == 15 | CountyCode == 17 | CountyCode == 25) |
           (StateCode == 35 & (CountyCode == 3 | CountyCode ==6 | CountyCode ==31 | CountyCode ==39 | CountyCode ==43 | CountyCode ==45)) |
           (StateCode == 08 & (CountyCode == 29 | CountyCode == 33 | CountyCode == 45 | CountyCode ==67 | CountyCode ==77 | CountyCode ==81 | CountyCode ==83 | CountyCode ==85 | CountyCode ==91 | CountyCode ==103 | CountyCode ==113)) |
           (StateCode == 49 & (CountyCode == 1 | CountyCode == 7 | CountyCode ==13 | CountyCode ==15 | CountyCode ==17 | CountyCode == 19 | CountyCode ==21 | CountyCode ==23 | CountyCode ==25 | CountyCode ==27 | CountyCode ==31 | CountyCode == 37 | CountyCode ==39 | CountyCode ==41 | CountyCode ==47 | CountyCode ==49 | CountyCode ==51 | CountyCode == 53 | CountyCode ==55))) 


#convert to wide format
wide<-dcast(cdef2, SiteID~CharacteristicName+ResultMeasureMeasureUnitCode, value.var="ResultMeasureValue", median)
#write to csv

write.csv(cdef2, file = "~/Desktop/20241024_Complete_WQP_DataMerge.csv", row.names = FALSE)
