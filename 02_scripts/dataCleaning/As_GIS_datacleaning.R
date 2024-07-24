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

data <- data %>%
  mutate(
    As3Cat = case_when(
      ResultMeasureValue > 10 ~ "C3",
      ResultMeasureValue > 5 ~ "C2",
      ResultMeasureValue >= 1 ~ "C1",
      TRUE ~ "C1"  # Values below 1 will also be C1
    )
  )

set.seed(1234)

data <- data[sample(1:nrow(data)), ]

# get the numb 70/30 training test split
#split into training (70%) and testing set (30%)
sample_set<-sample(nrow(data), round(nrow(data)*.7), replace = F)
data_train = data[sample_set,]
data_test = data[-sample_set,]

# Add the spl3cat column
data$spl3cat <- FALSE
data$spl3cat[sample_set] <- TRUE
table(data$spl3cat)

set.seed(1234)

data <- data[sample(1:nrow(data)), ]

# get the numb 70/30 training test split
#split into training (70%) and testing set (30%)
sample_set<-sample(nrow(data), round(nrow(data)*.7), replace = F)
data_train = data[sample_set,]
data_test = data[-sample_set,]

data$spl5 <- FALSE
data$spl5[sample_set] <- TRUE
table(data$spl5)


# - "US_L3NAME", -"ORIG_LABEL", "FID_Surficial_materials", "US_L3CODE", "UNIT_CODE"
data <- subset(data, select = -c(WellHoleDepthMeasureValue, Built, WellDepthMeasureUnitCode, US_L3NAME, WellHoleDepthMeasureUnitCode, UNIT_NAME, ResultMeasureMeasureUnitCode, ORIG_LABEL, ElevationUnitCode, FID_Surficial_materials, US_L3CODE, UNIT_CODE, GENERALIZE))

write.csv(data, file = "~/Desktop/Cleaned_As_GIS_Filtered.csv", row.names = FALSE)
