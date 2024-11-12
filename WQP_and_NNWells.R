#Combine NNwells and WQP data tables 

library(reshape2)
library(gtools)
library(dplyr)
library(tidyr)


setwd("/Volumes/HooverShare/Shared_Group_Data/20_projects/06_coPlateau_Rework/")

#Clean up the workspace
rm(list=ls())

WQP <- read.csv("./02_Data/Raw_Data/WQP_As_All.csv", na.strings = "NULL")
NNWells <- read.csv("./02_Data/Raw_Data/NNWells_As_All.csv", na.strings = "NULL")

As_COPlat_Data<-smartbind(WQP, NNWells)


#US_L3NAME
unique_vals <- unique(As_COPlat_Data$US_L3NAME)
for (val in unique_vals) {
 col_name <- paste("US_L3NAME", gsub(" ", "_", gsub(",", "", val)), sep = "_")
 As_COPlat_Data[[col_name]] <- ifelse(As_COPlat_Data$US_L3NAME == val, 1, 0)
}
table(As_COPlat_Data$US_L3NAME)
table(As_COPlat_Data$US_L3NAME_Southern_Rockies)

#UNIT_NAME
unique_vals <- unique(As_COPlat_Data$UNIT_NAME)
for (val in unique_vals) {
 col_name <- paste("UNIT_NAME", gsub(" ", "_", gsub(",", "", val)), sep = "_")
 As_COPlat_Data[[col_name]] <- ifelse(As_COPlat_Data$UNIT_NAME == val, 1, 0)
}
table(As_COPlat_Data$UNIT_NAME)
table(As_COPlat_Data$UNIT_NAME_Water)

#GENERALIZE
unique_vals <- unique(As_COPlat_Data$GENERALIZE)
for (val in unique_vals) {
 col_name <- paste("GENERALIZE", gsub(" ", "_", gsub(",", "", val)), sep = "_")
 As_COPlat_Data[[col_name]] <- ifelse(As_COPlat_Data$GENERALIZE == val, 1, 0)
}
table(As_COPlat_Data$GENERALIZE)
table(As_COPlat_Data$GENERALIZE_Water)

#Categorize wells by As conc.
As_COPlat_Data <- As_COPlat_Data %>%
  mutate(ClassLTE1 = ifelse(as.numeric(As) <= 1, 1, 2)) %>%
  mutate(ClassLTE3 = ifelse(as.numeric(As) <= 3, 1, 2)) %>%
  mutate(ClassLTE5 = ifelse(as.numeric(As) <= 5, 1, 2)) %>%
  mutate(ClassLTE10 = ifelse(as.numeric(As) <= 10, 1, 2)) %>%
  mutate(ClassGT10 = ifelse(as.numeric(As) > 10, 1, 2)) 
  

write.csv(As_COPlat_Data, file = "~/Desktop/All_As_Data.csv", row.names = FALSE)



