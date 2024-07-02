#setwd("C:/Users/jhoover/Documents/GitHub/coPlateauWaterQuality/01_data/CoPlateau")
setwd("/Users/austinmartinez/Documents/GitHub/coPlateauWaterQuality/01_data/CoPlateau")

data = read.csv("Combined-U.csv")

# Removed rows with "Total" in ResultSampleFractionText
filtered_data <- subset(data, ResultSampleFractionText != "Total")
#table(filtered_data$ResultSampleFractionText)

# took out rows without "U" in CharacteristicName
filtered_data <- filtered_data[filtered_data$CharacteristicName == "U", ]
#table(filtered_data$CharacteristicName)

# Replace all Null values in ResultMeasureValue column with 0.01
filtered_data$ResultMeasureValue[filtered_data$ResultMeasureValue == "NULL"] <- 0.01
#table(filtered_data$CharacteristicName["NULL"])

#made ResultMeasureValue numeric
filtered_data$ResultMeasureValue <- as.numeric(as.character(filtered_data$ResultMeasureValue))

# converted ResultMeasureValue from pCi/L to ug/L for rows where ResultMeasureMeasureUnitCode is pCi/L
pCiL_indices <- which(filtered_data$ResultMeasureMeasureUnitCode == "pCi/L")
filtered_data$ResultMeasureValue[pCiL_indices] <- filtered_data$ResultMeasureValue[pCiL_indices] / 0.67
# made all units ug/L
filtered_data$ResultMeasureMeasureUnitCode <- "ug/L"


# Save the combined data frame as a CSV file on your desktop
write.csv(filtered_data, file = "~/Desktop/Filtered_Combined-U.csv", row.names = FALSE)

