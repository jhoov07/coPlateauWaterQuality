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

#remove Arsenic NA's
As_COPlat_Data2<-As_COPlat_Data %>% drop_na(As)

#US_L3NAME
unique_vals <- unique(As_COPlat_Data2$US_L3NAME)
for (val in unique_vals) {
 col_name <- paste("US_L3NAME", gsub(" ", "_", gsub(",", "", val)), sep = "_")
 As_COPlat_Data2[[col_name]] <- ifelse(As_COPlat_Data2$US_L3NAME == val, 1, 0)
}
table(As_COPlat_Data2$US_L3NAME)
table(As_COPlat_Data2$US_L3NAME_Southern_Rockies)

#UNIT_NAME
unique_vals <- unique(As_COPlat_Data2$UNIT_NAME)
for (val in unique_vals) {
 col_name <- paste("UNIT_NAME", gsub(" ", "_", gsub(",", "", val)), sep = "_")
 As_COPlat_Data2[[col_name]] <- ifelse(As_COPlat_Data2$UNIT_NAME == val, 1, 0)
}
table(As_COPlat_Data2$UNIT_NAME)
table(As_COPlat_Data2$UNIT_NAME_Water)

#GENERALIZE
unique_vals <- unique(As_COPlat_Data2$GENERALIZE)
for (val in unique_vals) {
 col_name <- paste("GENERALIZE", gsub(" ", "_", gsub(",", "", val)), sep = "_")
 As_COPlat_Data2[[col_name]] <- ifelse(As_COPlat_Data2$GENERALIZE == val, 1, 0)
}
table(As_COPlat_Data2$GENERALIZE)
table(As_COPlat_Data2$GENERALIZE_Water)

write.csv(As_COPlat_Data2, file = "~/Desktop/All_As_Data.csv", row.names = FALSE)



