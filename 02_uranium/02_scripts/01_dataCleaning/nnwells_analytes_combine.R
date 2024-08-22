setwd("/Users/aaronnuanez/Documents/GitHub/coPlateauWaterQuality/02_uranium")

#Load libraries
#library(tidyverse)
library(dplyr)

data<-read.csv("./01_data/analytes.csv")
data3<-read.csv("./01_data/nnwells3_Check_ExportTable.csv")

#Filter to fields we want, add or remove fields as needed

data2 <- data %>%
  filter(analyte == "As" |
           analyte == "U" |
           analyte == "Ca" |
           analyte == "Alkalinity_Total" |
           analyte == "Carbonate" |
           analyte == "Fe" |
           analyte == "Nitrate" |
           analyte == "pH" |
           analyte == "Salinity" |
           

#Convert to wide format
dataW<-dcast(data2, well_id~analyte, value.var = "result", fun.aggregate = mean)

write.csv(combined_data, file = "~/Desktop/3combined_welldata.csv", row.names = FALSE)

