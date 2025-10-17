setwd("/Volumes/HooverShare/Shared_Group_Data/20_projects/06_coPlateau_Rework/")

datawells <- read.csv("./02_Data/Raw_Data/nnwells_redo.csv")
dataanalytes <- read.csv("./02_Data/Raw_Data/20241003_nnwellsAnalytes.csv")

merged_nnwells_tables <- merge(datawells, dataanalytes, by = "well_id", all.x=TRUE)

##Reorder
abc<-merged_nnwells_tables[,c(1,220:230,2:218)]

##Drop unused field
abc2<-abc[,-c(13:23, 26:30, 32:38, 40:56, 58:59, 229)]

#GENERALIZE, bedrock geology
unique_vals <- unique(data$GENERALIZE)
for (val in unique_vals) {
  col_name <- paste("GENERALIZE", gsub(" ", "_", gsub(",", "", val)), sep = "_")
  data[[col_name]] <- ifelse(data$GENERALIZE == val, 1, 0)
}

#UNIT_NAME
unique_vals <- unique(data$UNIT_NAME)
for (val in unique_vals) {
  col_name <- paste("UNIT_NAME", gsub(" ", "_", gsub(",", "", val)), sep = "_")
  data[[col_name]] <- ifelse(data$UNIT_NAME == val, 1, 0)
}

#US_L3NAME
unique_vals <- unique(data$US_L3NAME)
for (val in unique_vals) {
  col_name <- paste("US_L3NAME", gsub(" ", "_", gsub(",", "", val)), sep = "_")
  data[[col_name]] <- ifelse(data$US_L3NAME == val, 1, 0)
}

write.csv(abc2, file = "~/Desktop/101024MergedNNwells.csv", row.names = FALSE)

