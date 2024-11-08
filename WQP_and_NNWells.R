#Combine NNwells and WQP data tables 

library(gtools)

setwd("/Volumes/HooverShare/Shared_Group_Data/20_projects/06_coPlateau_Rework/")

#Clean up the workspace
rm(list=ls())

WQP <- read.csv("./02_Data/Raw_Data/WQP_As_All.csv", na.strings = "NULL")
NNWells <- read.csv("./02_Data/Raw_Data/NNWells_As_All.csv", na.strings = "NULL")

As_COPLat_Data<-smartbind(WQP, NNWells)

write.csv(As_COPLat_Data, file = "~/Desktop/All_As_Data.csv", row.names = FALSE)



