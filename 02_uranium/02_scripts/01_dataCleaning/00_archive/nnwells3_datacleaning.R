setwd("~/Desktop")
data = read.csv("nnwells3_ExportTable.csv")

newdata <- data[complete.cases(data$depth), ]

#US_L3NAME
unique_vals <- unique(newdata$US_L3NAME)
for (val in unique_vals) {
  col_name <- paste("US_L3NAME", gsub(" ", "_", gsub(",", "", val)), sep = "_")
  newdata[[col_name]] <- ifelse(newdata$US_L3NAME == val, 1, 0)
}
table(newdata$US_L3NAME)
table(newdata$US_L3NAME_Southern_Rockies)

#UNIT_NAME
unique_vals <- unique(newdata$UNIT_NAME)
for (val in unique_vals) {
  col_name <- paste("UNIT_NAME", gsub(" ", "_", gsub(",", "", val)), sep = "_")
  newdata[[col_name]] <- ifelse(newdata$UNIT_NAME == val, 1, 0)
}
table(newdata$UNIT_NAME)
table(newdata$UNIT_NAME_Water)

#GENERALIZE
unique_vals <- unique(newdata$GENERALIZE_1)
for (val in unique_vals) {
  col_name <- paste("GENERALIZE_1", gsub(" ", "_", gsub(",", "", val)), sep = "_")
  newdata[[col_name]] <- ifelse(newdata$GENERALIZE_1 == val, 1, 0)
}
table(newdata$GENERALIZE_1)
table(newdata$GENERALIZE_1_Water)

#removes columns from 'newdata'
newdata <- subset(newdata, select = -c(US_L3CODE, ORIG_LABEL_1, UNIT_CODE, UNIT_NAME, US_L3NAME, GENERALIZE_1))

write.csv(newdata, file = "~/Desktop/Clean_nnwells3_ExportTable.csv", row.names = FALSE)
