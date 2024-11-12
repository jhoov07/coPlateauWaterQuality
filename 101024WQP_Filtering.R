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
wide<-dcast(cdef2, SiteID~CharacteristicName, value.var="ResultMeasureValue", median)
#write.csv(wide, file = "~/Desktop/wide.csv", row.names = FALSE)

#Load in NN Wells table so you have the right fields
#nnwells<-read.csv("./02_Data/Raw_Data/WQP/00_archive/Clean_nnwells3_ExportTable.csv", na.strings = "NULL")


#Read GIS data from WQP
i<-read.csv("./02_Data/Raw_Data/WQP/00_archive/20241105_WQP_Export2.csv", na.strings = "NULL")

#Clean up new dataframe, drop worthless fields
cleani<- i [-c(1:25,27:54,56:59,61:68,70:79)]
#write.csv(cleani, file = "~/Desktop/cleani.csv", row.names = FALSE)

#Return unique records
cleanNoDup<-distinct(cleani[,c(1:174)])  #only use 
#write.csv(cleanNoDup, file = "~/Desktop/cleanNoDup.csv", row.names = FALSE)


#class(cleanNoDup[["SiteID"]])
#class(wide[["SiteID"]])
#Merge with wide using SiteID
WQP_All<-merge(wide, cleanNoDup, by="SiteID", all.x=TRUE)

#Ensure WQP Merge table and NNwells table have same column names 

#Import and merge As data with WQP_All
j<-read.csv("./02_Data/Raw_Data/WQP/00_archive/AZ_Arsenic.csv", na.strings = "NULL")
k<-read.csv("./02_Data/Raw_Data/WQP/00_archive/CO_Arsenic.csv", na.strings = "NULL")
l<-read.csv("./02_Data/Raw_Data/WQP/00_archive/UT_Arsenic.csv", na.strings = "NULL")
m<-read.csv("./02_Data/Raw_Data/WQP/00_archive/NM_Arsenic.csv", na.strings = "NULL")

#Merge As files into a single data frame
as<-rbind(j,k,l,m)
#write.csv(as, file = "~/Desktop/as.csv", row.names = FALSE)


#Process As data
as2 <- as %>%
  drop_na(ResultMeasureMeasureUnitCode) %>%
  filter(ResultSampleFractionText == "Dissolved") %>%
  filter(ResultMeasureMeasureUnitCode != "ratio") %>%
  filter(CharacteristicName == "As")

summary(factor(as2$ResultMeasureMeasureUnitCode)) #Check to see what units are noted in the field
mgL_indices <- which(as2$ResultMeasureMeasureUnitCode == "mg/L") #Create an index with records that we need to convert
as2$ResultMeasureValue[mgL_indices] <- as2$ResultMeasureValue[mgL_indices] / 1000 #Convert to mg/L to match NN Wells analyte data
as2$ResultMeasureMeasureUnitCode <- "ug/L"   # made all units mg/L
#write.csv(as2, file = "~/Desktop/as2.csv", row.names = FALSE)

as3 <- as2 %>%
  filter(StateCode == 4 & (CountyCode == 1 | CountyCode == 5 | CountyCode == 7 | CountyCode == 15 | CountyCode == 17 | CountyCode == 25) |
           (StateCode == 35 & (CountyCode == 3 | CountyCode ==6 | CountyCode ==31 | CountyCode ==39 | CountyCode ==43 | CountyCode ==45)) |
           (StateCode == 08 & (CountyCode == 29 | CountyCode == 33 | CountyCode == 45 | CountyCode ==67 | CountyCode ==77 | CountyCode ==81 | CountyCode ==83 | CountyCode ==85 | CountyCode ==91 | CountyCode ==103 | CountyCode ==113)) |
           (StateCode == 49 & (CountyCode == 1 | CountyCode == 7 | CountyCode ==13 | CountyCode ==15 | CountyCode ==17 | CountyCode == 19 | CountyCode ==21 | CountyCode ==23 | CountyCode ==25 | CountyCode ==27 | CountyCode ==31 | CountyCode == 37 | CountyCode ==39 | CountyCode ==41 | CountyCode ==47 | CountyCode ==49 | CountyCode ==51 | CountyCode == 53 | CountyCode ==55))) 
#write.csv(as3, file = "~/Desktop/as3.csv", row.names = FALSE)

#Convert to Wide
wide_as3<-dcast(as3, SiteID~CharacteristicName, value.var="ResultMeasureValue", median)
#write.csv(wide_as3, file = "~/Desktop/wide_as3.csv", row.names = FALSE)
#class(wide_as3[["SiteID"]])

#Merge with WQP_All using SiteID
WQP_As_All<-merge(WQP_All, wide_as3, by="SiteID", all.x=TRUE)

WQP_As_All_reorder <- WQP_As_All %>% 
  select(1:6, "As", everything())

#rename columns and drop As NA's
WQP2<- WQP_As_All_reorder %>% 
  rename(baseflow = bfi48grd_ProjectRaster,
         prism30yr = PRISM_ppt_30yr_ProjectRaster) %>%
  drop_na(As)

#add WQP identifying column
WQP2$Data_Source <- "WQP"

#Keep only As, Fe, pH to match NNWells 
WQP3 <- WQP2 [, -c(2, 3, 6)]

#write to csv
write.csv(WQP3, file = "~/Desktop/WQP_As_All.csv", row.names = FALSE)

