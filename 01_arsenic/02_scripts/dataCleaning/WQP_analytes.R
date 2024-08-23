setwd("/Users/hoover/desktop/data")

#Load libraries
#library(tidyverse)
library(dplyr)

#Load data from WQP
Fe <- read.csv("iron.csv")
Ca <- read.csv("Ca.csv")
Alkalinity <- read.csv("Alkalinity.csv")
pH <- read.csv("pH.csv")

#Load existing wQp data file that's already been cleaned
data <-read.csv()

data2 <- data %>%
  filter(analyte == "As" |
  
  
  
  
  
write.csv(combined_data, file = "~/Desktop/#combined_analytes_nnwells.csv", row.names = FALSE)
