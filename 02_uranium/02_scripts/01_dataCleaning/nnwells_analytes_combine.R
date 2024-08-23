setwd("/Users/hoover/Documents/GitHub/coPlateauWaterQuality/02_uranium")

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
           analyte == "Conductivity")
           

#Convert to wide format
dataW<-dcast(data2, 
             well_id~analyte, 
             value.var = "result", 
             fun.aggregate = mean)

#Merge by well_id or sample_id?
dataM <- merge(data3[,-c(7:9)], dataW, by = "well_id", all = TRUE)  ##Drop the U, As and Fl data in Data 3 since they seem short for some reason

<<<<<<< Updated upstream

write.csv(combined_data, file = "~/Desktop/20combined_welldata.csv", row.names = FALSE)
=======
write.csv(combined_data, file = "~/Desktop/combined_analytes_nnwells.csv", row.names = FALSE)
>>>>>>> Stashed changes

