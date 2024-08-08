#setwd("/Users/aaronnuanez/Documents/GitHub/coPlateauWaterQuality")
setwd("/Users/hoover/Documents/GitHub/coPlateauWaterQuality")

install.packages("dplyr")
install.packages("readr")

#Load libraries
library(tidyverse)


#Clean up the workspace
rm(list=ls())


#Load data
data <- read.csv("./02_uranium/01_data/Clean_Nure6_Data_ExportTable.csv")
data2 <- read.csv("./01_arsenic/01_data/Cleaned_As_GIS_Filtered.csv")
data3 <- read.csv("./02_uranium/01_data/Clean_nnwells3_ExportTable.csv")

#Rename fields in NURE dataset
data<- data %>% 
  rename(baseflow = bfi48grd_ProjectRaster2,
    prism30yr = PRISM_ppt_30yr_ProjectRaster1,
    U = u_fl_ppb,)

#Rename fields in NN Wells Data
data2<- data2 %>% 
  rename(baseflow = bfi48grd,
         prism30yr = PRISM_30yrNorm,
         welldpth = WellDepthMeasureValue)

#Rename fields in data from Saurav
data3<- data3 %>% 
  rename(welldpth = depth,
         As = As_)


#Need to find missing fields then create new blank fields, i started by comparing data and data1, need to repeat for data2
a <- colnames(data)
b <- colnames(data2)
c <- colnames(data3)

setdiff(a, b) #this will tell you the fields that don't match between a and b, anything not matching needs to be added to b
setdiff(b, a) #this will tell you the fields that don't match between b and a, anything not matching needs to be added to a
setdiff(a,c)
setdiff(c,a)
setdiff(b,c)
setdiff(c,b)


#Add in missing columns so all datasets have the same columns. 
df <- read.csv("./02_uranium/01_data/Clean_Nure6_Data_ExportTable.csv")
df$CharacteristicName <- NA
df$ResultSampleFractionText <- NA
df$ResultMeasureValue <- NA
#write updated data fram back to a CSV file
write.csv(df,"./02_uranium/01_data/Clean_Nure6_Data_ExportTable.csv")

df3 <- read.csv("./02_uranium/01_data/Clean_nnwells3_ExportTable.csv")
df3$CharacteristicName <- NA
df3$u_fl_ppb <- NA
df3$ResultSampleFractionText <- NA
df3$ResultMeasureValue <- NA
write.csv(df3,"./02_uranium/01_data/Clean_nnwells3_ExportTable.csv")

df2 <- read.csv("./01_arsenic/01_data/Cleaned_As_GIS_Filtered.csv")
df2$u_fl_ppb <- NA

write.csv(df2, "./01_arsenic/01_data/Cleaned_As_GIS_Filtered.csv")

