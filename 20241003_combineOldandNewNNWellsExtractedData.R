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

write.csv(dataM, file = "~/Desktop/dataM.csv", row.names=FALSE)

#Load WQP data, then clean and merge
#wqp<-read.csv("./02_Data/Raw_Data/202410105_WQP_As_All.csv")

#rename columns 
#dataM<- dataM %>% 
 # rename(SiteID = well_id)

#x<-smartbind(wqp, dataM)

#write.csv(sort(colnames(wqp)))
#write.csv(sort(colnames(dataM)))




#write.csv(dataM, file = "~/Desktop/nnwells_merge.csv", row.names = FALSE)


#US_L3NAME
#unique_vals <- unique(dataM$US_L3NAME)
#for (val in unique_vals) {
 # col_name <- paste("US_L3NAME", gsub(" ", "_", gsub(",", "", val)), sep = "_")
  #dataM[[col_name]] <- ifelse(dataM$US_L3NAME == val, 1, 0)
#}
#table(dataM$US_L3NAME)
#table(dataM$US_L3NAME_Southern_Rockies)

#UNIT_NAME
#unique_vals <- unique(dataM$UNIT_NAME)
#for (val in unique_vals) {
 # col_name <- paste("UNIT_NAME", gsub(" ", "_", gsub(",", "", val)), sep = "_")
#  dataM[[col_name]] <- ifelse(dataM$UNIT_NAME == val, 1, 0)
#}
#table(dataM$UNIT_NAME)
#table(dataM$UNIT_NAME_Water)

#GENERALIZE
#unique_vals <- unique(dataM$GENERALIZE_1)
#for (val in unique_vals) {
 # col_name <- paste("GENERALIZE_1", gsub(" ", "_", gsub(",", "", val)), sep = "_")
  #dataM[[col_name]] <- ifelse(dataM$GENERALIZE_1 == val, 1, 0)
#}
#table(dataM$GENERALIZE_1)
#table(dataM$GENERALIZE_1_Water)


#rename columns
newdata<- dataM %>% 
  rename(SiteID = well_id)

#Delete rows where OID_ = NA since there is no well or geochem data, just analyte data
#newdata4 <- subset(newdata3, OID_ != "NA")


write.csv(newdata, file = "~/Desktop/NNWells_As_All.csv", row.names = FALSE)

