#setwd("~/Desktop")
#setwd("/Users/aaronnuanez/Documents/GitHub/coPlateauWaterQuality")
setwd("/Users/hoover/Documents/GitHub/coPlateauWaterQuality")

#Load libraries
library(tidyverse)
library(dplyr)

#Clean up the workspace
rm(list=ls())

#Load data
data <- read.csv("./02_uranium/01_data/Nure7_Data_ExportTable.csv")
data2 <- read.csv("./02_uranium/01_data/wqpData_cleaned_20240808.csv", na.strings = "NULL")
data3 <- read.csv("./02_uranium/01_data/nnwells3_ExportTable.csv")

#Load file to correct NN Wells data frame field names
newNNWellsColnames<-read.csv("./02_uranium/02_scripts/01_dataCleaning/newNNWellsColNames_Aug9.csv", check.names = FALSE)

#Rename fields in NURE dataset
data<- data %>% 
  rename(baseflow = bfi48grd_ProjectRaster2, prism30yr = PRISM_ppt_30yr_ProjectRaster1, U = u_fl_ppb)

#Rename fields in NN Wells Data
data2<- data2 %>% 
  rename(baseflow = bfi48grd,
         prism30yr = PRISM_30yrNorm,
         welldpth = WellDepthMeasureValue,
         As = ResultMeasureValue)

#Rename fields in data from Saurav
data3<- data3 %>% 
  rename(welldpth = depth,
         As = As_,
         Fl = Fl_combine)

##Concatenate data source name to SiteID/WellID/Record Number
data$SiteID<-paste("nure-",data$rec_no, sep="")
data2$SiteID<-paste("wtrQulPort-",data2$SiteID, sep="")
data3$SiteID<-paste("nnWells-",data3$well_id, sep="")

abc<-as.vector(unlist(newNNWellsColnames))

#Assign updated colnames
colnames(data3)<-abc

#combine data sets
combined_data <- bind_rows(data, data2, data3)

# removes specified columns
combined_data <- subset(combined_data, select = -c(u_dn_ppb, OID_, OID1, Elevation, rec_no, CharacteristicName, ResultSampleFractionText, well_id, code, bas1, bas5, bas10, As3Cat, spl3cat, spl5))

#Reorder data slightly for easier modeling
combinedData_reorder<-combined_data[,c(180, 1, 2, 181, 218, 3:179, 182:217)]

#Drop a few other fields
combinedData_reorder2<-combinedData_reorder[,-c(6,7, 177:180, 211,212)]

#Change NAs to 0 for dummy variables 177-221, 223-232
# Replace NAs with 0s


write.csv(combined_data, file = "~/Desktop/2combined_welldata.csv", row.names = FALSE)




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

