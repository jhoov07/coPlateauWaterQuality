#setwd("~/Desktop")
#setwd("/Users/aaronnuanez/Documents/GitHub/coPlateauWaterQuality")
#etwd()
#setwd("/Volumes/HooverShare/Shared_Group_Data/20_projects/06_coPlateau_Rework/")
setwd("~/Desktop")

#Load libraries
library(tidyverse)
library(dplyr)
library(reshape2)

#Clean up the workspace
rm(list=ls())

#Load data
data = read.csv("analytes.csv")

dataS<- data %>%
  filter(analyte == "As" |
           analyte== "U" |
           analyte == "pH" |
           analyte == "Ca" |
           analyte == "SO4-" |
           analyte == "Si" |
           analyte == "B" |
           analyte == "SpecificConductance" |
           analyte == "Fe" |
           analyte == "Alkalinity" |
           analyte == "Bicarbonate" |
           analyte == "Carbonate" |
           analyte=="Na" |
           analyte =="Mg")

summary(factor(dataS$analyte))

#Turn wide format
dataW<-dcast(dataS, well_id~analyte, median, value.var = "result")

write.csv(dataW, file="20241003_nnwellsData.csv")