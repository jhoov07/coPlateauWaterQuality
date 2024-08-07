setwd("~/Desktop")
data = read.csv("Nure6_Data_ExportTable.csv")

#remove rows where 'welldpth' column has NA values
newdata <- na.omit(data, cols = "welldpth")

#removes u_dn_ppb from newdata 
newdata <- newdata[,-4]

# Remove rows where 'u_fl_ppb' column has NA values
newdata <- na.omit(newdata, cols = "u_fl_ppb")

#GENERALIZE
unique_vals <- unique(newdata$GENERALIZE)
for (val in unique_vals) {
  col_name <- paste("GENERALIZE", gsub(" ", "_", gsub(",", "", val)), sep = "_")
  newdata[[col_name]] <- ifelse(newdata$GENERALIZE == val, 1, 0)
}
table(newdata$GENERALIZE)
table(newdata$GENERALIZE_Water)

#UNIT_NAME
unique_vals <- unique(newdata$UNIT_NAME)
for (val in unique_vals) {
  col_name <- paste("UNIT_NAME", gsub(" ", "_", gsub(",", "", val)), sep = "_")
  newdata[[col_name]] <- ifelse(newdata$UNIT_NAME == val, 1, 0)
}
table(newdata$UNIT_NAME)
table(newdata$UNIT_NAME_Water)


#US_L3NAME
unique_vals <- unique(newdata$US_L3NAME)
for (val in unique_vals) {
  col_name <- paste("US_L3NAME", gsub(" ", "_", gsub(",", "", val)), sep = "_")
  newdata[[col_name]] <- ifelse(newdata$US_L3NAME == val, 1, 0)
}
table(newdata$US_L3NAME)
table(newdata$US_L3NAME_Madrean_Archipelago)

#removes columns from 'newdata'
newdata <- subset(newdata, select = -c(US_L3CODE, ORIG_LABEL, UNIT_CODE, UNIT_NAME, US_L3NAME, GENERALIZE))

write.csv(newdata, file = "~/Desktop/Clean_Nure6_Data_ExportTable.csv", row.names = FALSE)
