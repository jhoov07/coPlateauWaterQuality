#Spatial prediction
wd <- ("/Users/hoover/desktop/")

# Open your reference Raster
#Reference_Raster <- list.files(paste0(wd,"15VariablesforXGB10ugL"), full.names=TRUE, pattern = "Fe_500m.tif$")
#Reference_Raster <- raster(Reference_Raster)

# Open the other raster
#Raster <- list.files(paste0(wd,"15VariablesforXGB10ugL"), full.names=TRUE, pattern=".tif$")
#Raster <- raster(Raster)

## ----list-files-tif--------------------------------------------------
rasterlist2 <-  list.files(paste0(wd,"15VariablesforXGB10ugL"), full.names=TRUE, pattern=".tif$")
rasterlist2

library(terra)

#Load each raster to check extent and crop as needed
A_Aragon<-rast("/Users/hoover/desktop/15VariablesforXGB10ugL/A_Aragon_500m.tif")
A_Calcite<-rast("/Users/hoover/desktop/15VariablesforXGB10ugL/A_Calcite_500m.tif" )
A_Cs<-rast("/Users/hoover/desktop/15VariablesforXGB10ugL/A_Cs_500m.tif" )
A_Tot_14A<-rast("/Users/hoover/desktop/15VariablesforXGB10ugL/A_Tot_14A.tif" )

C_Amorph<-rast("/Users/hoover/desktop/15VariablesforXGB10ugL/C_Amorph.tif")
C_Analcime<-rast("/Users/hoover/desktop/15VariablesforXGB10ugL/C_Analcime.tif")
C_Cr<-rast("/Users/hoover/desktop/15VariablesforXGB10ugL/C_Cr_500m.tif")
C_Hematite<-rast("/Users/hoover/desktop/15VariablesforXGB10ugL/C_Hematite_500m.tif")
C_Mo<-rast("/Users/hoover/desktop/15VariablesforXGB10ugL/C_Mo.tif")

Fe<-rast("/Users/hoover/desktop/15VariablesforXGB10ugL/fe_v2.tif" )
pH<-rast("/Users/hoover/desktop/15VariablesforXGB10ugL/pH_v2.tif" )
prism30yr<-rast("/Users/hoover/desktop/15VariablesforXGB10ugL/prism30yr_500m.tif" )
Top5_Ca<-rast("/Users/hoover/desktop/15VariablesforXGB10ugL/Top5_Ca.tif")
Top5_S<-rast("/Users/hoover/desktop/15VariablesforXGB10ugL/Top5_S_500m.tif")

DepthToGW<-rast("/Users/hoover/desktop/15VariablesforXGB10ugL/DepthToGW_V2.tif")

#Plot rasters to make sure they look correct
plot(A_Aragon)
plot(A_Calcite)
plot(A_Cs)
plot(A_Tot_14A)
plot(C_Amorph)

plot(C_Analcime)
plot(C_Cr)
plot(C_Hematite)
plot(C_Mo)
plot(Fe)

plot(pH)
plot(prism30yr)
plot(Top5_Ca)
plot(Top5_S)
plot(DepthToGW)

#Change names so they match the XGB model
A_Aragon@data@names<-"A_Aragon"
A_Calcite@data@names<-"A_Calcite"
A_Cs@data@names<-"A_Cs"
A_Tot_14A@data@names<-"A_Tot_14A"

C_Amorph@data@names<-"C_Amorph"
C_Analcime@data@names<-"C_Analcime"
C_Cr@data@names<-"C_Cr"
C_Hematite@data@names<-"C_Hematite"
C_Mo@data@names<-"C_Mo"

Fe@data@names<-"Fe"
pH@data@names<-"pH"
prism30yr@data@names<-"prism30yr"
Top5_Ca@data@names<-"Top5_Ca"
Top5_S@data@names<-"Top5_S"

DepthToGW@data@names<-"DepthToGW"


#Resample
A_Aragon<-resample(A_Aragon, Fe, method = "bilinear")
A_Calcite<-resample(A_Calcite, Fe, method = "bilinear")
A_Cs<-resample(A_Cs, Fe, method = "bilinear")
A_Tot_14A<-resample(A_Tot_14A, Fe, method = "bilinear")
C_Amorph<-resample(C_Amorph, Fe, method = "bilinear")
C_Analcime<-resample(C_Analcime, Fe, method = "bilinear")
C_Cr<-resample(C_Cr, Fe, method = "bilinear")
C_Hematite<-resample(C_Hematite, Fe, method = "bilinear")
C_Mo<-resample(C_Mo, Fe, method = "bilinear")
prism30yr <- resample(prism30yr, Fe, method = "bilinear")
Top5_Ca <- resample(Top5_Ca, Fe, method = "bilinear")
Top5_S <- resample(Top5_S, Fe, method = "bilinear")

pH <- resample(pH, Fe, method = 'bilinear')
#Fe<- resample(Fe, Top5_S, method = 'bilinear')
DepthToGW <- resample(DepthToGW, Fe, method = 'bilinear')

#crop
A_Aragon<-crop(A_Aragon, Fe, mask = TRUE)
A_Calcite<-crop(A_Calcite, Fe, mask = TRUE)
A_Cs<-crop(A_Cs, Fe, mask = TRUE)
A_Tot_14A<-crop(A_Tot_14A, Fe, mask = TRUE)
C_Amorph<-crop(C_Amorph, Fe, mask = TRUE)
C_Analcime<-crop(C_Analcime, Fe, mask = TRUE)
C_Cr<-crop(C_Cr, Fe, mask = TRUE)
C_Hematite<-crop(C_Hematite, Fe, mask = TRUE)
C_Mo<-crop(C_Mo, Fe, mask = TRUE)
prism30yr <- crop(prism30yr, Fe, mask = TRUE)
Top5_Ca <- crop(Top5_Ca, Fe, mask = TRUE)
Top5_S <- crop(Top5_S, Fe, mask = TRUE)

pH <- crop(pH, Fe, mask = TRUE)
#Fe<- resample(Fe, Top5_S, method = 'bilinear')
DepthToGW <- crop(DepthToGW, Fe, mask = TRUE)

#Write cropped rasters to file
writeRaster(A_Aragon, "/Users/hoover/desktop/updatedRasters10ugLAs/A_Aragon.tif")
writeRaster(A_Calcite, "/Users/hoover/desktop/updatedRasters10ugLAs/A_Calcite.tif")
writeRaster(A_Cs, "/Users/hoover/desktop/updatedRasters10ugLAs/A_Cs.tif")
writeRaster(A_Tot_14A, "/Users/hoover/desktop/updatedRasters10ugLAs/A_Tot_14A.tif")
writeRaster(C_Amorph, "/Users/hoover/desktop/updatedRasters10ugLAs/C_Amorph.tif")

writeRaster(C_Analcime, "/Users/hoover/desktop/updatedRasters10ugLAs/C_Analcime.tif")
writeRaster(C_Cr, "/Users/hoover/desktop/updatedRasters10ugLAs/C_Cr.tif")
writeRaster(C_Hematite, "/Users/hoover/desktop/updatedRasters10ugLAs/C_Hematite.tif")
writeRaster(C_Mo, "/Users/hoover/desktop/updatedRasters10ugLAs/C_Mo.tif")
writeRaster(prism30yr, "/Users/hoover/desktop/updatedRasters10ugLAs/prism30yr.tif")

writeRaster(Top5_Ca, "/Users/hoover/desktop/updatedRasters10ugLAs/Top5_Ca.tif")
writeRaster(Top5_S, "/Users/hoover/desktop/updatedRasters10ugLAs/Top5_S.tif")
writeRaster(pH, "/Users/hoover/desktop/updatedRasters10ugLAs/pH.tif")
writeRaster(DepthToGW, "/Users/hoover/desktop/updatedRasters10ugLAs/DepthToGW.tif")

writeRaster(Fe, "/Users/hoover/desktop/updatedRasters10ugLAs/Fe.tif")
