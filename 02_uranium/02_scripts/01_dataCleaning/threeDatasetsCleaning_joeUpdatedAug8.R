#setwd("~/Desktop")
setwd("/Users/aaronnuanez/HooverShare/Shared Group Data/20_projects/05_coPlateau")

#Load libraries
library(tidyverse)
library(dplyr)

#Clean up the workspace
rm(list=ls())

#Load data
Nure <- read.csv("./02_uranium/01_data/Nure7_Data_ExportTable.csv")
WQP <- read.csv("./02_uranium/01_data/wqpData_cleaned_20240808.csv", na.strings = "NULL")
NNwells <- read.csv("./02_uranium/01_data/nnwells3_Check_ExportTable.csv")

#Rename fields in NURE dataset
Nure <- Nure %>% 
  rename(baseflow = bfi48grd_ProjectRaster2, prism30yr = PRISM_ppt_30yr_ProjectRaster1, U = u_fl_ppb)

#Rename fields in WQP Data
WQP<- WQP %>% 
  rename(baseflow = bfi48grd,
         prism30yr = PRISM_30yrNorm,
         welldpth = WellDepthMeasureValue,
         As = ResultMeasureValue,
         F30mElevationFoCo = F30mElevat)

#Rename fields in NNwells data
NNwells<- NNwells %>% 
  rename(welldpth = depth,
         As = As_,
         Fl = Fl_combine)
#table(data3$Fl)
##Concatenate data source name to SiteID/WellID/Record Number
Nure$SiteID<-paste("nure-",Nure$rec_no, sep="")
WQP$SiteID<-paste("nnWells-",WQP$SiteID, sep="")
NNwells$SiteID<-paste("wtrQalPort-",NNwells$well_id, sep="")

#Update field types for a few variables so they match
#data$SiteID <- as.numeric(data$SiteID)
#data2$SiteID <- as.numeric(data2$SiteID)
#data3$SiteID <- as.numeric(data3$SiteID)

#str(NNwells$SiteID)
#combine data sets
combined_data <- bind_rows(Nure, WQP)

combined_data1 <- bind_rows(combined_data, NNwells)#

# removes specified columns
combined_data <- subset(combined_data, select = -c(OID_, rec_no, CharacteristicName, ResultSampleFractionText, well_id, code, bas1, bas5, bas10, As3Cat, spl3cat, spl5))

#Reorder data slightly for easier modeling
combinedData_reorder<-combined_data[,c(218, 2, 219, 222, 220, 1, 3:217, 221, 223:232 )]

#Change NAs to 0 for dummy variables 177-221, 223-232
# Replace NAs with 0s


write.csv(combined_data, file = "~/Desktop/3combined_welldata.csv", row.names = FALSE)


#Need to find missing fields then create new blank fields, i started by comparing data and data1, need to repeat for data2
a<-colnames(Nure)
b<-colnames(WQP)
c<-colnames(NNwells)

#setdiff(a, b) #this will tell you the fields that don't match between a and b, anything not matching needs to be added to b, shows what data is in a that is not in b
#setdiff(b, a) #this will tell you the fields that don't match between b and a, anything not matching needs to be added to a
#setdiff(a, c)
#setdiff(b, c)
#setdiff(c, a)
#setdiff(c, b)


#Add in missing columns so all datasets have the same columns. 
#df <- data
#df$CharacteristicName <- NA
#df$ResultSampleFractionText <- NA
#df$ResultMeasureValue <- NA
#drop rec_no
#df = df[,-2]
#write updated data fram back to a CSV file
#write.csv(df,"~/Desktop/2Clean_Nure6_Data_ExportTable.csv", row.names = FALSE)

#df3 <- data2
#df3$CharacteristicName <- NA
#df3$u_fl_ppb <- NA
#df3$ResultSampleFractionText <- NA
#df3$ResultMeasureValue <- NA
#drop ResultSampleFractionText and characteristicname
#df3 = df3[,-2]
#df3 = df3[,-2]
#write.csv(df3,"~/Desktop/2Clean_nnwells3_ExportTable.csv", row.names = FALSE)

##df2 <- data3
#df2$u_fl_ppb <- NA
#drop u_fl_ppb
#df2 = df2[,-204]
#write.csv(df2, "~/Desktop/2Cleaned_As_GIS_Filtered.csv", row.names = FALSE)

#nnwells = read.csv("2Clean_nnwells3_ExportTable.csv")
#Nure6 = read.csv("2Clean_Nure6_Data_ExportTable.csv")
#Asdata = read.csv("2Cleaned_As_GIS_Filtered.csv")

#nnwells$welldpth <- as.numeric(nnwells$welldpth)
#Nure6$welldpth <- as.numeric(Nure6$welldpth)
#Asdata$welldpth <- as.numeric(Asdata$welldpth)

