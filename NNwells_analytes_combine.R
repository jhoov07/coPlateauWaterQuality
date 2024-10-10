setwd("/Volumes/HooverShare/Shared_Group_Data/20_projects/06_coPlateau_Rework/")

datawells <- read.csv("./02_Data/Raw_Data/nnwells_redo.csv")
dataanalytes <- read.csv("./02_Data/Raw_Data/20241003_nnwellsAnalytes.csv")

merged_nnwells_tables <- merge(datawells, dataanalytes, by = "well_id", all.x=TRUE)

#Omit rows where no NNwell data is present
#omit_merged_nnwells_tables <- subset(merged_nnwells_tables, OID_ != "NA")

#Need to omit rows where no Analytes data is present 

##Reorder
abc<-merged_nnwells_tables[,c(1,220:230,2:218)]

##Drop unused field
abc2<-abc[,-c(13:23, 26:30, 32:38, 40:56, 58:59, 229)]


write.csv(merged_nnwells_tables, file = "~/Desktop/100924MergedNNwells.csv", row.names = FALSE)

