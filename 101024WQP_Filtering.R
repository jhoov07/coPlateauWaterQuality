#Merge WQP data tables

#Load libraries
library(reshape2)
library(dplyr)

#Set working directory
setwd("/Volumes/HooverShare/Shared_Group_Data/20_projects/06_coPlateau_Rework/")

#Load csv files
a<-read.csv("./02_Data/Raw_Data/WQP/00_archive/AZ_Uranium.csv")
b<-read.csv("./02_Data/Raw_Data/WQP/00_archive/NM_Uranium.csv")
c<-read.csv("./02_Data/Raw_Data/WQP/00_archive/Ca.csv", na.strings = "NULL")
d<-read.csv("./02_Data/Raw_Data/WQP/00_archive/Iron.csv", na.strings = "NULL")
e<-read.csv("./02_Data/Raw_Data/WQP/00_archive/ph.csv", na.strings = "NULL")
f<-read.csv("./02_Data/Raw_Data/WQP/00_archive/alkalinity.csv", na.strings = "NULL")

#Process the files
f2<- f %>%
  filter(ResultSampleFractionText == "Total") %>%
  mutate()

#for alkalinity and pH: check activity media name is water, check result sample fraction text to be "total", check result measure measure unit code is "std units", if anything else drop it 
#Alkalinity: filter to only "total" in ResultSampleFractionText, convert ug/L to mg/L in ResultMeasureMeasureUnitCode
#pH:filter to only "total" in ResultSampleFractionText, remove "NULL" in ResultMeasureMeasureUnitCode

#for iron, calcium, Uranium: check activity media name is water, check result sample fraction text to be "dissolved", check result measure measure unit code is "ug/L" (see if units make sense then do unit conversions)
#Iron: filter to only dissolved in ResultSampleFractionText,
#Calcium: filter to only dissolved in ResultSampleFractionText, keep mg/L
#Uranium: AZ (keep "U" and "Uranium-238" in CharacteristicName), NM (keep "U" and "Uranium-238" in CharacteristicName)


#Merge files
cdef<-rbind(c,d,e,f)

length(unique(cdef$SiteID))

#convert to wide format
wide<-dcast(cdef, SiteID~CharacteristicName+ResultMeasureMeasureUnitCode, value.var="ResultMeasureValue", max)
#write to csv

write.csv("", file = "~/Desktop/.csv", row.names = FALSE)
