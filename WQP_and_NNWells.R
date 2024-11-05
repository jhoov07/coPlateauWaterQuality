#Combine NNwells and WQP data tables 


setwd("/Volumes/HooverShare/Shared_Group_Data/20_projects/06_coPlateau_Rework/")

#Clean up the workspace
rm(list=ls())

WQP <- read.csv("./02_Data/Raw_Data/WQP_As_All.csv", na.strings = "NULL")
NNWells <- read.csv("./02_Data/Raw_Data/Clean_nnwells3_ExportTable.csv", na.strings = "NULL")

As_COPLat_Data<-rbind(WQP, NNWells)



