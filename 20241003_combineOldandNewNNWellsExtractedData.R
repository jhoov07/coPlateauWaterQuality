setwd("/Volumes/HooverShare/Shared_Group_Data/20_projects/06_coPlateau_Rework/")

#Load libraries
library(reshape2)
library(dplyr)
library(tidyr)

dataOld <- read.csv("./02_Data/Raw_Data/NNWells/nnwells3_Check_ExportTable.csv")
dataNew <- read.csv("./02_Data/Raw_Data/NNWells/20241003_nnwellsData.csv")

#Merge data tables
dataM<-merge(dataOld[,-c(7:9)], dataNew, by="well_id", all.y=TRUE)

#write.csv(dataM, file = "~/Desktop/nnwells_merge.csv", row.names = FALSE)


#US_L3NAME
unique_vals <- unique(dataM$US_L3NAME)
for (val in unique_vals) {
  col_name <- paste("US_L3NAME", gsub(" ", "_", gsub(",", "", val)), sep = "_")
  dataM[[col_name]] <- ifelse(dataM$US_L3NAME == val, 1, 0)
}
table(dataM$US_L3NAME)
table(dataM$US_L3NAME_Southern_Rockies)

#UNIT_NAME
unique_vals <- unique(dataM$UNIT_NAME)
for (val in unique_vals) {
  col_name <- paste("UNIT_NAME", gsub(" ", "_", gsub(",", "", val)), sep = "_")
  dataM[[col_name]] <- ifelse(dataM$UNIT_NAME == val, 1, 0)
}
table(dataM$UNIT_NAME)
table(dataM$UNIT_NAME_Water)

#GENERALIZE
unique_vals <- unique(dataM$GENERALIZE_1)
for (val in unique_vals) {
  col_name <- paste("GENERALIZE_1", gsub(" ", "_", gsub(",", "", val)), sep = "_")
  dataM[[col_name]] <- ifelse(dataM$GENERALIZE_1 == val, 1, 0)
}
table(dataM$GENERALIZE_1)
table(dataM$GENERALIZE_1_Water)

#removes columns from 'dataM'
newdata <- subset(dataM, select = -c(US_L3CODE, ORIG_LABEL_1, UNIT_CODE, UNIT_NAME, US_L3NAME, GENERALIZE_1))

#rename columns
newdata2<- newdata %>% 
  rename(SiteID = well_id,
         SO4 = SO4.) 

newdata3<-subset(newdata2, select = -c(F30mElevationFoCo, X, Best_Guess__ac_))


#add NNWells identifying column
newdata3$Data_Source <- "NNWells"

write.csv(newdata3, file = "~/Desktop/Clean_nnwells3_ExportTable.csv", row.names = FALSE)

