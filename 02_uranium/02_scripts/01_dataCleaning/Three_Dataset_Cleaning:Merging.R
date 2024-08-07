setwd("/Users/aaronnuanez/Documents/GitHub/coPlateauWaterQuality")
data = read.csv("Clean_Nure6_Data_ExportTable.csv")


data <- read.csv("./02_uranium/01_data/Clean_Nure6_Data_ExportTable.csv")
data2 <- read.csv("./01_arsenic/01_data/Cleaned_As_GIS_Filtered.csv")
data3 <- read.csv("./02_uranium/01_data/Clean_nnwells3_ExportTable.csv")

#Rename columns so all datasets match
data <- colnames(bfi48grd_ProjectRaster2, baseflow)
data2 <- colnames(bfi48grd, baseflow)
data <- colnames(PRISM_ppt_30yr_ProjectRaster1, prism30yr)
data2 <- colnames(PRISM_30yrNorm, prism30yr)
data2 <- colnames(WellDepthMeasureValue, welldpth)
data3 <- colnames(depth, welldpth)

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
