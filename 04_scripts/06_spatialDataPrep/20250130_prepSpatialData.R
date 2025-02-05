#Spatial prediction layer prep 
#Steps
#Create raster list
#import as spatial raster
#Align
#Crop
#Write to file

#10 ug/L model, Arsenic
#Key variables - "A_Aragon_500m.tif"   "A_Calcite_500m.tif"  "A_Cs_500m.tif"       "A_Tot_14A.tif"       "C_Amorph.tif"        "C_Analcime.tif"     
#[7] "C_Cr_500m.tif"       "C_Hematite_500m.tif" "C_Mo.tif"            "DepthToGW_V2.tif"    "fe_v2.tif"           "pH_v2.tif"          
#[13] "prism30yr_500m.tif"  "Top5_Ca.tif"         "Top5_S_500m.tif" 

#5 ug/L model
#[1] "A_C_Tot.tif"        "A_Calcite.tif"      "A_Hg.tif"           "A_Kaolinit.tif"     "A_Quartz.tif"       "A_Tl.tif"          
#[7] "A_Tot_Flds.tif"     "C_Cr.tif"           "C_Hematite.tif"     "C_Kaolinit.tif"     "C_Sb.tif"           "C_Se.tif"          
#[13] "C_Tot_14A.tif"      "DepthToGW_500m.tif" "Fe_500m.tif"        "pH_500m.tif"        "prism30yr_500m.tif" "Top5_As.tif"       
#[19] "Top5_Ba.tif"        "Top5_Ca.tif"  

rm(list=ls()) #clean up the workspace
wd <- ("/Users/hoover/desktop/tifsForModeling") #write using local folder since the files are too large for GitHub


## ----list-files-tif--------------------------------------------------
rasterlist2 <-  list.files(wd, full.names=FALSE, pattern=".tif$")
rasterlist2

library(terra)

#Load each raster to check extent and crop as needed
A_Aragon<-rast(paste(wd,"/A_Aragon_500m.tif", sep=""))
A_C_Tot<-rast(paste(wd,"/A_C_Tot.tif", sep=""))
A_Calcite<-rast(paste(wd, "/A_Calcite_500m.tif", sep=""))
A_Cs<-rast(paste(wd,"/A_Cs_500m.tif", sep=""))
A_Hg<-rast(paste(wd,"/A_Hg.tif", sep=""))
A_Kaolinit<-rast(paste(wd,"/A_Kaolinit.tif", sep=""))
A_Quartz<-rast(paste(wd,"/A_Quartz.tif", sep=""))
A_Tl<-rast(paste(wd,"/A_Tl.tif", sep=""))
A_Tot_14A<-rast(paste(wd,"/A_Tot_14A.tif", sep=""))
A_Tot_Flds<-rast(paste(wd,"/A_Tot_Flds.tif", sep=""))

C_Amorph<-rast(paste(wd,"/C_Amorph.tif", sep=""))
C_Analcime<-rast(paste(wd,"/C_Analcime.tif", sep=""))
C_Cr<-rast(paste(wd,"/C_Cr_500m.tif", sep=""))
C_Hematite<-rast(paste(wd,"/C_Hematite_500m.tif", sep=""))
C_Kaolinit<-rast(paste(wd,"/C_Kaolinit.tif", sep=""))
C_Mo<-rast(paste(wd,"/C_Mo.tif", sep=""))
C_Sb<-rast(paste(wd,"/C_Sb.tif", sep=""))
C_Se<-rast(paste(wd,"/C_Se.tif", sep=""))
C_Tot_14A<-rast(paste(wd,"/C_Tot_14A.tif", sep=""))

DepthToGW<-rast(paste(wd,"/DepthToGW_V2.tif", sep=""))
Fe<-rast(paste(wd,"/fe_v2.tif" , sep=""))
pH<-rast(paste(wd,"/pH_v2.tif" , sep=""))
prism30yr<-rast(paste(wd,"/prism30yr_500m.tif", sep=""))

Top5_As<-rast(paste(wd,"/Top5_As.tif", sep=""))
Top5_Ba<-rast(paste(wd,"/Top5_Ba.tif", sep=""))
Top5_Ca<-rast(paste(wd,"/Top5_Ca.tif", sep=""))
Top5_S<-rast(paste(wd,"/Top5_S_500m.tif", sep=""))

#<-rast(paste(wd,".tif", sep=""))

#Resample
A_Aragon<-resample(A_Aragon, Fe, method = "bilinear")
A_C_Tot<-resample(A_C_Tot, Fe, method = "bilinear")
A_Calcite<-resample(A_Calcite, Fe, method = "bilinear")
A_Cs<-resample(A_Cs, Fe, method = "bilinear")
A_Hg<-resample(A_Hg, Fe, method = "bilinear")
A_Kaolinit<-resample(A_Kaolinit, Fe, method = "bilinear")
A_Quartz<-resample(A_Quartz, Fe, method = "bilinear")
A_Tl<-resample(A_Tl, Fe, method = "bilinear")
A_Tot_14A<-resample(A_Tot_14A, Fe, method = "bilinear")
A_Tot_Flds<-resample(A_Tot_Flds, Fe, method = "bilinear")

C_Amorph<-resample(C_Amorph, Fe, method = "bilinear")
C_Analcime<-resample(C_Analcime, Fe, method = "bilinear")
C_Cr<-resample(C_Cr, Fe, method = "bilinear")
C_Hematite<-resample(C_Hematite, Fe, method = "bilinear")
C_Kaolinit<-resample(C_Kaolinit, Fe, method = "bilinear")
C_Mo<-resample(C_Mo, Fe, method = "bilinear")
C_Sb<-resample(C_Sb, Fe, method = "bilinear")
C_Se<-resample(C_Se, Fe, method = "bilinear")
C_Tot_14A<-resample(C_Tot_14A, Fe, method = "bilinear")

DepthToGW<-resample(DepthToGW, Fe, method = "bilinear")
pH<-resample(pH, Fe, method = "bilinear")
prism30yr<-resample(prism30yr, Fe, method = "bilinear")

Top5_As<-resample(Top5_As, Fe, method = "bilinear")
Top5_Ba<-resample(Top5_Ba, Fe, method = "bilinear")
Top5_Ca<-resample(Top5_Ca, Fe, method = "bilinear")
Top5_S<-resample(Top5_S, Fe, method = "bilinear")


#crop
A_Aragon<-crop(A_Aragon, Fe, mask = TRUE)
A_C_Tot<-crop(A_C_Tot, Fe, mask = TRUE)
A_Calcite<-crop(A_Calcite, Fe, mask = TRUE)
A_Cs<-crop(A_Cs, Fe, mask = TRUE)
A_Hg<-crop(A_Hg, Fe, mask = TRUE)
A_Kaolinit<-crop(A_Kaolinit, Fe, mask = TRUE)
A_Quartz<-crop(A_Quartz, Fe, mask = TRUE)
A_Tl<-crop(A_Tl, Fe, mask = TRUE)
A_Tot_14A<-crop(A_Tot_14A, Fe, mask = TRUE)
A_Tot_Flds<-crop(A_Tot_Flds, Fe, mask = TRUE)

C_Amorph<-crop(C_Amorph, Fe, mask = TRUE)
C_Analcime<-crop(C_Analcime, Fe, mask = TRUE)
C_Cr<-crop(C_Cr, Fe, mask = TRUE)
C_Hematite<-crop(C_Hematite, Fe, mask = TRUE)
C_Kaolinit<-crop(C_Kaolinit, Fe, mask = TRUE)
C_Mo<-crop(C_Mo, Fe, mask = TRUE)
C_Sb<-crop(C_Sb, Fe, mask = TRUE)
C_Se<-crop(C_Se, Fe, mask = TRUE)
C_Tot_14A<-crop(C_Tot_14A, Fe, mask = TRUE)

DepthToGW<-crop(DepthToGW, Fe, mask = TRUE)
pH<-crop(pH, Fe, mask = TRUE)
prism30yr<-crop(prism30yr, Fe, mask = TRUE)

Top5_As<-crop(Top5_As, Fe, mask = TRUE)
Top5_Ba<-crop(Top5_Ba, Fe, mask = TRUE)
Top5_Ca<-crop(Top5_Ca, Fe, mask = TRUE)
Top5_S<-crop(Top5_S, Fe, mask = TRUE)




#Write cropped rasters to file
d<-"/Users/hoover/desktop/spatialPredFormattedTifs/"

writeRaster(A_Aragon, paste(d, "A_Aragon.tif", sep=""))
writeRaster(A_C_Tot, paste(d, "A_C_Tot.tif", sep=""))
writeRaster(A_Calcite, paste(d, "A_Calcite.tif", sep=""))
writeRaster(A_Cs, paste(d, "A_Cs.tif", sep=""))

writeRaster(A_Hg, paste(d, "A_Hg.tif", sep=""))
writeRaster(A_Kaolinit, paste(d, "A_Kaolinit.tif", sep=""))
writeRaster(A_Quartz, paste(d, "A_Quartz.tif", sep=""))
writeRaster(A_Tl, paste(d, "A_Tl.tif", sep=""))
writeRaster(A_Tot_14A, paste(d, "A_Tot_14A.tif", sep=""))

writeRaster(A_Tot_Flds, paste(d, "A_Tot_Flds.tif", sep=""))
writeRaster(C_Amorph, paste(d, "C_Amorph.tif", sep=""))
writeRaster(C_Analcime, paste(d, "C_Analcime.tif", sep=""))
writeRaster(C_Cr, paste(d, "C_Cr.tif", sep=""))
writeRaster(C_Hematite, paste(d, "C_Hematite.tif", sep=""))

writeRaster(C_Kaolinit, paste(d, "C_Kaolinit.tif", sep=""))
writeRaster(C_Mo, paste(d, "C_Mo.tif", sep=""))
writeRaster(C_Sb, paste(d, "C_Sb.tif", sep=""))
writeRaster(C_Se, paste(d, "C_Se.tif", sep=""))
writeRaster(C_Tot_14A, paste(d, "C_Tot_14A.tif", sep=""))

writeRaster(DepthToGW, paste(d, "DepthToGW.tif", sep=""))
writeRaster(pH, paste(d, "pH.tif", sep=""))
writeRaster(prism30yr, paste(d, "prism30yr.tif", sep=""))
writeRaster(Top5_As, paste(d, "Top5_As.tif", sep=""))
writeRaster(Top5_Ba, paste(d, "Top5_Ba.tif", sep=""))

writeRaster(Top5_Ca, paste(d, "Top5_Ca.tif", sep=""))
writeRaster(Top5_S, paste(d, "Top5_S.tif", sep=""))

#Reference raster
writeRaster(Fe, paste(d, "Fe.tif", sep=""))


#writeRaster(, paste(d, ".tif", sep=""))




