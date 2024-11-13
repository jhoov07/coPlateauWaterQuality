setwd("/Volumes/HooverShare/Shared_Group_Data/20_projects/06_coPlateau_Rework/")

#Load libraries
library(reshape2)
library(dplyr)
library(tidyr)
library(gtools)

#Clean up the workspace
rm(list=ls())

#NN Wells data
dataGIS <- read.csv("./02_Data/Raw_Data/NNWells/nnwells_redo2.csv")
dataWQ <- read.csv("./02_Data/Raw_Data/NNWells/20241003_nnwellsData.csv")

#Merge data tables
dataM<-merge(dataGIS[,-c(1:4, 6:20, 22:28, 30:44, 46:47)], dataWQ[,-c(1,5,6,8,11,12,13)], by="well_id", all.y=TRUE)
dataM$Data_Source<-"NNWells"

dataM2 <- dataM [,-c(175, 177, 180)]

dataM2<- dataM2 %>% 
  filter(well_id <= 7458) %>%
  drop_na(As) 

write.csv(dataM2, file = "~/Desktop/dataM.csv", row.names=FALSE)

#rename columns
newdata<- dataM2 %>% 
  rename(SiteID = well_id,
         prism30yr = PRISM_ppt_30yr_ProjectRaster,
         baseflow = bfi48grd_ProjectRaster,
         DepthToGW = conus_MF6_SS_U_ProjectRaster)

#Delete rows where OID_ = NA since there is no well or geochem data, just analyte data
#newdata4 <- subset(newdata3, OID_ != "NA")


write.csv(newdata, file = "~/Desktop/NNWells_As_All.csv", row.names = FALSE)

