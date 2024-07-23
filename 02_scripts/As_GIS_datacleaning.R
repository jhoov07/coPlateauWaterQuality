setwd("~/Desktop")
data = read.csv("As_GIS_Filtered3.csv")


#ResultSampleFractionText
table(data$ResultSampleFractionText) #all dissolved

#ElevationUnitCode
table(data$ElevationUnitCode) #all feet or null



#GENERALIZE
unique_vals <- unique(data$GENERALIZE)
for (val in unique_vals) {
  col_name <- paste("GENERALIZE", gsub(" ", "_", gsub(",", "", val)), sep = "_")
  data[[col_name]] <- ifelse(data$GENERALIZE == val, 1, 0)
}
table(data$GENERALIZE)
table(data$GENERALIZE_Igneous_intrusive)


#UNIT_NAME
unique_vals <- unique(data$UNIT_NAME)
for (val in unique_vals) {
  col_name <- paste("UNIT_NAME", gsub(" ", "_", gsub(",", "", val)), sep = "_")
  data[[col_name]] <- ifelse(data$UNIT_NAME == val, 1, 0)
}
table(data$UNIT_NAME)
table(data$UNIT_NAME_Water)


#US_L3NAME
unique_vals <- unique(data$US_L3NAME)
for (val in unique_vals) {
  col_name <- paste("US_L3NAME", gsub(" ", "_", gsub(",", "", val)), sep = "_")
  data[[col_name]] <- ifelse(data$US_L3NAME == val, 1, 0)
}
table(data$US_L3NAME)
table(data$US_L3NAME_Central_Basin_and_Range)



data <- data %>%
  mutate(
    bas1 = ifelse(ResultMeasureValue > 1, 1, 0),
    bas5 = ifelse(ResultMeasureValue > 5, 1, 0),
    bas10 = ifelse(ResultMeasureValue > 10, 1, 0)
  )

# - "US_L3NAME", -"ORIG_LABEL", "FID_Surficial_materials", "US_L3CODE", "UNIT_CODE ect."
data <- subset(data, select = -c(WellHoleDepthMeasureValue, Built, WellDepthMeasureUnitCode, US_L3NAME, WellHoleDepthMeasureUnitCode, UNIT_NAME, ResultMeasureMeasureUnitCode, ORIG_LABEL, ElevationUnitCode, FID_Surficial_materials, US_L3CODE, UNIT_CODE, GENERALIZE))

write.csv(data, file = "~/Desktop/Cleaned_As_GIS_Filtered.csv", row.names = FALSE)
