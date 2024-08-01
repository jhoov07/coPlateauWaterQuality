library(tidyverse)

#setwd("~/Desktop")
setwd("/Users/hoover/Documents/GitHub/coPlateauWaterQuality/02_uranium")

#Clean up the workspace
rm(list=ls())

#data = read.csv("./01_data/U_GIS_Filtered.csv"). #I dont' know where this file lives right now, I don't think it's on github
data = read.csv("./01_data/Cleaned_U_GIS_Filtered.csv") #I dont' know where this file lives right now, I don't think it's on github


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

#Create U occurence categories
data <- data %>%
  mutate(
    bU3 = ifelse(ResultMeasureValue > 3, 1, 0),
    bU15 = ifelse(ResultMeasureValue > 15, 1, 0),
    bU30 = ifelse(ResultMeasureValue > 30, 1, 0)
  )

data <- data %>%
    mutate(
      U3Cat = case_when(
        ResultMeasureValue > 30 ~ "C3",
        ResultMeasureValue > 15 ~ "C2",
        ResultMeasureValue >= 3 ~ "C1",
        TRUE ~ "C1"  # Values below 1 will also be C1
    )
  )

set.seed(1234)

#split into training (70%) and testing set (30%), keep training set balances with overall distribution
sample_set<-sample.split(data$U3Cat, SplitRatio = 0.7)

data <- data %>%
  mutate(
    trainCat3 = ifelse(sample_set == TRUE, 1, 0)
  )

#Create split for BU15
#data$spl5 <- FALSE
#data$spl5[sample_set] <- TRUE
#table(data$spl5)


# - "US_L3NAME", -"ORIG_LABEL", "FID_Surficial_materials", "US_L3CODE", "UNIT_CODE"
data <- subset(data, select = -c(WellHoleDepthMeasureValue, Built, WellDepthMeasureUnitCode, US_L3NAME, WellHoleDepthMeasureUnitCode, UNIT_NAME, ResultMeasureMeasureUnitCode, ORIG_LABEL, ElevationUnitCode, FID_Surficial_materials_Project, US_L3CODE, UNIT_CODE, GENERALIZE))

write.csv(data, file = "./01_data/Cleaned_U_GIS_Filtered_Joe.csv", row.names = FALSE)
