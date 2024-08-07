#setwd("/Users/aaronnuanez/Documents/GitHub/coPlateauWaterQuality")
setwd("/Users/hoover/Documents/GitHub/coPlateauWaterQuality")

#Load libraries
library(tidyverse)

#Clean up the workspace
rm(list=ls())


#Load data
data <- read.csv("./02_uranium/01_data/Clean_Nure6_Data_ExportTable.csv")
data2 <- read.csv("./01_arsenic/01_data/Cleaned_As_GIS_Filtered.csv")
data3 <- read.csv("./02_uranium/01_data/Clean_nnwells3_ExportTable.csv")

#Rename fields in NURE dataset
data<- data %>% 
  rename(baseflow = bfi48grd_ProjectRaster2, prism30yr = PRISM_ppt_30yr_ProjectRaster1, U = u_fl_ppb) %>%
    mutate(GENERALIZE_Metamorphic_gneiss = 0) %>%
    mutate(GENERALIZE_Metamorphic_volcanic = 0)

#Rename fields in NN Wells Data
data2<- data2 %>% 
  rename(baseflow = bfi48grd,
         prism30yr = PRISM_30yrNorm,
         welldpth = WellDepthMeasureValue,
         As = ResultMeasureValue)

#Rename fields in data from Saurav
data3<- data3 %>% 
  rename(welldpth = depth,
         As = As_,
         Fl = Fl_combined)

##Concatenate data source name to SiteID/WellID/Record Number
data$SiteID<-paste("nure-",data$rec_no, sep="")
data2$SiteID<-paste("nnWells-",data2$SiteID, sep="")
data3$SiteID<-paste("wtrQalPort-",data3$well_id, sep="")

#Need to find missing fields then create new blank fields, i started by comparing data and data1, need to repeat for data2
a<-colnames(data)
b<-colnames(data2)
c<-colnames(data3)

setdiff(a, b) #this will tell you the fields that don't match between a and b, anything not matching needs to be added to b
setdiff(b, a) #this will tell you the fields that don't match between b and a, anything not matching needs to be added to a
setdiff(a, c)
setdiff(b, c)
setdiff(c, a)

#Notes
#Nure data - drop rec_no, rename u_fl_ppb as U, add dummary variables with NAs for the remaining fields
#"GENERALIZE_Metamorphic_gneiss"                       "GENERALIZE_Metamorphic_volcanic"                    
#[5] "GENERALIZE_Metamorphic_sedimentary_clastic"          "GENERALIZE_Igneous_and_Metamorphic_undifferentiated"
#[7] "GENERALIZE_Metamorphic_schist"                       "UNIT_NAME_Lacustrine_sediments"                     
#[9] "UNIT_NAME_Eolian_sediments_on_southern_High_Plains"  "US_L3NAME_Madrean_Archipelago"                      
#[11] "US_L3NAME_Chihuahuan_Deserts"                        "US_L3NAME_Southwestern_Tablelands"                  
#[13] "US_L3NAME_High_Plains"                               "US_L3NAME_Northern_Basin_and_Range"   


#[3] "GENERALIZE_Sedimentary_clastic"                             "GENERALIZE_Igneous_volcanic"                               
#[5] "GENERALIZE_Unconsolidated_and_Sedimentary_undifferentiated" "GENERALIZE_Igneous_and_Sedimentary_undifferentiated"       
#[7] "GENERALIZE_Sedimentary_undifferentiated"                    "GENERALIZE_Igneous_intrusive"                              
#[9] "GENERALIZE_Metamorphic_and_Sedimentary_undifferentiated"    "GENERALIZE_Metamorphic_undifferentiated"                   
#[11] "GENERALIZE_Metamorphic_gneiss"                              "GENERALIZE_Metamorphic_volcanic"                           
#[13] "GENERALIZE_Metamorphic_sedimentary_clastic"                 "GENERALIZE_Sedimentary_carbonate"                          
#[15] "GENERALIZE_Igneous_and_Metamorphic_undifferentiated"        "GENERALIZE_Water"                                          
#[17] "GENERALIZE_Metamorphic_schist"                              "UNIT_NAME_Playa_sediments"                                 
#[19] "UNIT_NAME_Rhyolitic_volcanic_rocks"                         "UNIT_NAME_Lacustrine_sediments"                            
#[21] "UNIT_NAME_Eolian_sediments_on_southern_High_Plains"         "UNIT_NAME_Glacial_till_sediments_mostly_sandy_thin"        
#[23] "US_L3NAME_Madrean_Archipelago"                              "US_L3NAME_Chihuahuan_Deserts"                              
#[25] "US_L3NAME_Sonoran_Basin_and_Range"                          "US_L3NAME_Southwestern_Tablelands"                         
#[27] "US_L3NAME_Mojave_Basin_and_Range"                           "US_L3NAME_Wasatch_and_Uinta_Mountains"                     
#[29] "US_L3NAME_Central_Basin_and_Range"                          "US_L3NAME_High_Plains"                                     
#[31] "US_L3NAME_Northern_Basin_and_Range"                        

#WQP data - Drop characteristic name, drop ResultSampleFractionText, add elevation, add LM_C_Zn_7_18, drop rest of the fields






#Add in missing columns so all datasets have the same columns. 
df <- read.csv("./02_uranium/01_data/Clean_Nure6_Data_ExportTable.csv")
df$CharacteristicName <- NA
df$ResultSampleFractionText <- NA
df$ResultMeasureValue <- NA
#write updated data fram back to a CSV file
write.csv(df,"./02_uranium/01_data/Clean_Nure6_Data_ExportTable.csv")

df3 <- read.csv("./02_uranium/01_data/Clean_nnwells3_ExportTable.csv")
df3$CharacteristicName <- NA
df3$u_fl_ppb <- NA
df3$ResultSampleFractionText <- NA
df3$ResultMeasureValue <- NA
write.csv(df3,"./02_uranium/01_data/Clean_nnwells3_ExportTable.csv")

df2 <- read.csv("./01_arsenic/01_data/Cleaned_As_GIS_Filtered.csv")
df2$u_fl_ppb <- NA

write.csv(df2, "./01_arsenic/01_data/Cleaned_As_GIS_Filtered.csv")
