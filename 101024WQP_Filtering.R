#Merge WQP data tables

#Load libraries
library(reshape2)
library(dplyr)

#Set working directory
setwd("/Volumes/HooverShare/Shared_Group_Data/20_projects/06_coPlateau_Rework/")

#Load csv files
a<-read.csv("./02_Data/Raw_Data/WQP/00_archive/AZ_Uranium.csv")
b<-read.csv("./02_Data/Raw_Data/WQP/00_archive/NM_Uranium.csv")
c<-read.csv("./02_Data/Raw_Data/WQP/00_archive/Ca.csv", na.strings = "NULL")
d<-read.csv("./02_Data/Raw_Data/WQP/00_archive/Iron.csv", na.strings = "NULL")
e<-read.csv("./02_Data/Raw_Data/WQP/00_archive/ph.csv", na.strings = "NULL")
f<-read.csv("./02_Data/Raw_Data/WQP/00_archive/alkalinity.csv", na.strings = "NULL")

#Process the files

#Alkalinity: convert ug/L to mg/L in ResultMeasureMeasureUnitCode (need to do)
f2 <- f %>%
  filter(ResultSampleFractionText == "Total") %>%
  mutate() 
f3 <- f2 %>% filter(ResultMeasureMeasureUnitCode != "NULL")
#ug_to_mg <-0.001
#f4 <- f3 %>%
#  mutate(
#    ResultMeasureValue = ifelse(ResultMeasureMeasureUnitCode == "ug/L", ResultMeasureValue * ug_to_mg, ResultMeasureValue)  # Convert values
#    ResultMeasureMeasureUnitCode = ifelse(ResultMeasureMeasureUnitCode == "ug/L", "mg/L", ResultMeasureMeasureUnitCode)
#write.csv(f4, file = "~/Desktop/alkalinitycheck.csv", row.names = FALSE)

#pH 
e2 <- e %>%
  filter(ResultSampleFractionText == "Total") %>%
  mutate() 
e3 <- e2 %>% filter(ResultMeasureMeasureUnitCode != "NULL")
#write.csv(e3, file = "~/Desktop/pHcheck.csv", row.names = FALSE)

#Iron: unit conversions (need to do)
d2 <- d %>%
  filter(ResultSampleFractionText == "Total") %>%
  mutate() 
#write.csv(d2, file = "~/Desktop/ironcheck.csv", row.names = FALSE)

#Calcium: keep mg/L (need to do)
c2 <- c %>%
  filter(ResultSampleFractionText == "Dissolved") %>%
  mutate() 
write.csv(c2, file = "~/Desktop/calciumcheck.csv", row.names = FALSE)

#Uranium: AZ (keep "U" and "Uranium-238" in CharacteristicName), NM (keep "U" and "Uranium-238" in CharacteristicName)
a2 <- a %>% 
  filter(CharacteristicName == "Uranium-238" |
        CharacteristicName == "U") %>%
  mutate() 
a3 <- a2 %>% filter(ResultMeasureValue != "NULL")
b2 <- b %>%
  filter(CharacteristicName == "Uranium-238" |
        CharacteristicName == "U") %>%
  mutate()
b3 <- b2 %>% filter(ResultMeasureValue != "NULL")
write.csv(a3, file = "~/Desktop/AZ_Ucheck.csv", row.names = FALSE)
write.csv(b3, file = "~/Desktop/NM_Ucheck.csv", row.names = FALSE)


#Merge files
cdef<-rbind(c,d,e,f)

length(unique(cdef$SiteID))

#convert to wide format
wide<-dcast(cdef, SiteID~CharacteristicName+ResultMeasureMeasureUnitCode, value.var="ResultMeasureValue", max)
#write to csv

write.csv("", file = "~/Desktop/.csv", row.names = FALSE)
