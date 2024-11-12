setwd("/Volumes/HooverShare/Shared_Group_Data/20_projects/06_coPlateau_Rework/")

#Load libraries
library(reshape2)
library(dplyr)
library(tidyr)
library(gtools)

#Clean up the workspace
rm(list=ls())

#NN Wells data
dataGIS <- read.csv("./02_Data/Raw_Data/NNWells/nnwells_redo.csv")
dataWQ <- read.csv("./02_Data/Raw_Data/NNWells/20241003_nnwellsData.csv")

#Merge data tables
dataM<-merge(dataGIS[,-c(1:3, 5:19, 21:28, 30:45, 47:48)], dataWQ[,-c(1,5,6,8,11,12,13)], by="well_id", all.y=TRUE)
dataM$Data_Source<-"NNWells"

dataM2 <- dataM [,-c(174, 176, 179)]

dataM2<- dataM2 %>% 
  filter(well_id <= 7458) %>%
  drop_na(As) 

#write.csv(dataM2, file = "~/Desktop/dataM.csv", row.names=FALSE)

#Load WQP data, then clean and merge
#wqp<-read.csv("./02_Data/Raw_Data/202410105_WQP_As_All.csv")


#x<-smartbind(wqp, dataM)


#rename columns
newdata<- dataM2 %>% 
  rename(SiteID = well_id,
         prism30yr = PRISM_ppt_30yr_ProjectRaster,
         baseflow = bfi48grd_ProjectRaster)

#Delete rows where OID_ = NA since there is no well or geochem data, just analyte data
#newdata4 <- subset(newdata3, OID_ != "NA")


write.csv(newdata, file = "~/Desktop/NNWells_As_All.csv", row.names = FALSE)

