library(caTools) 
library(randomForest)
library(caret)
library(tidyverse)


setwd("/Users/hoover/Documents/GitHub/coPlateauWaterQuality/03_data/")
#setwd("/Users/aaronnuanez/Documents/GitHub/coPlateauWaterQuality/03_data")

rm(list=ls())

# set data and seed values
date<-Sys.Date()
set.seed(1234)  # Setting seed 

#Load data
Asdata <- read.csv("All_As_Data.csv")

# Filter data into train and test sets based on logical variable 'trainCat2'
train <- Asdata[Asdata$trainClassLTE5_splt == TRUE, ] #Need up update this field and dataframe to match what is produce in lines 21-24
test <- Asdata[Asdata$trainClassLTE5_splt == FALSE, ] #Need up update this field and dataframe to match what is produce in lines 21-24

#Make SiteID the row name so we can drop that field
rownames(train)<-paste(train$SiteID, train$Data_Source, sep="_")
rownames(test)<-paste(test$SiteID, test$Data_Source, sep="_")


#Drop unused fields
AsTrain<-train[,-c(1, 4, 109:112, 157:159, 161:168)] #Drop the As concentration, and the categorical variables we already transformed
AsTest<-test[,-c(1, 4, 109:112, 157:159, 161:168)]

AsTrain_y<-AsTrain[,151]
AsTest_y<-AsTest[,151]

#method indicates these variables(field indexes): 25, 2, 3, 105, 1, 104, 9, 97, 36, 58
# A_Calcite, pH, prismy30yr, C_Tot_K_fs, Fe, C_Tot_14A, Top5_Ca, C_Hematite, A_Kaolinit, A_Tot_Flds

#Keep only useful predictors from variable selection step
AsTrain<-AsTrain[,c(25, 2, 3, 105, 1, 104, 9, 97, 36, 58, 151)] #Drop the As concentration, and the categorical variables we already transformed
AsTest<-AsTest[,c(25, 2, 3, 105, 1, 104, 9, 97, 36, 58, 151)]

#Ensure ClassLTE5 is a Factor (Categorical Variable)
AsTrain$ClassLTE5 <- as.factor(AsTrain$ClassLTE5)
AsTest$ClassLTE5  <- as.factor(AsTest$ClassLTE5)

#Run random forest model
#mtry is from step 1, might want to try different number of trees too
model<-randomForest(data=AsTrain, factor(ClassLTE5)~., mtry=116, ntree=500, importance = TRUE); 
print(model)

#Partial dependence plots to help us sort out the impact on an individual variable on As concentrations
partialPlot(model, AsTest, A_Calcite, "1", xlab="A_Calcite", ylab="As Class", lwd=4, col="green")
partialPlot(model, AsTest, pH, "1", xlab="pH", ylab="As Class", lwd=4, col="green")
partialPlot(model, AsTest, prism30yr, "1", xlab="prism30yr", ylab="As Class", lwd=4, col="green")
partialPlot(model, AsTest, C_Tot_K_fs, "1", xlab="C_Tot_K_fs", ylab="As Class", lwd=4, col="green")
partialPlot(model, AsTest, Fe, "1", xlab="Fe", ylab="As Class", lwd=4, col="green")
partialPlot(model, AsTest, C_Tot_14A, "1", xlab="C_Tot_14A", ylab="As Class", lwd=4, col="green")
partialPlot(model, AsTest, Top5_Ca, "1", xlab="Top5_Ca", ylab="As Class", lwd=4, col="green")
partialPlot(model, AsTest, C_Hematite, "1", xlab="C_Hematite", ylab="As Class", lwd=4, col="green")
partialPlot(model, AsTest, A_Kaolinit, "1", xlab="A_Kaolinit", ylab="As Class", lwd=4, col="green")
partialPlot(model, AsTest, A_Tot_Flds, "1", xlab="A_Tot_Flds", ylab="As Class", lwd=4, col="green")

# Predicting the Test set results 
y_pred <- predict(model, newdata = AsTest)

# Confusion Matrix 
confusion_mtx <- confusionMatrix(y_pred, factor(AsTest$ClassLTE5), positive ="1")
confusion_mtx


#Testing Data
rfpred <- data.frame(predict (model, AsTest, type="prob"))

#Adjust the "true" threshold using Youden value
#For a figure
y_predJoin<-data.frame(cbind(AsTest_y, y_pred ,rfpred))#change field to match outcome modeled, this applies to LT10

#rename fields for ease of use
colnames(y_predJoin)[1]<-"Obsclass"
colnames(y_predJoin)[2]<-"PredClass"
colnames(y_predJoin)[3]<-"ProbNotexceed"
colnames(y_predJoin)[4]<-"ProbExceed"

y_predJoin$SiteInfo<-as.factor(rownames(y_predJoin))

y_predJoin$DataSource<-NA
y_predJoin$DataSource[grep("NNWells", y_predJoin$SiteInfo)]<-"NNWells"
y_predJoin$DataSource[grep("WQP", y_predJoin$SiteInfo)]<-"WQP"
summary(factor(y_predJoin$DataSource))

#Site ID
y_predJoin$SiteID<-NA
y_predJoin$SiteID<-as.numeric(gsub("\\D", "", y_predJoin$SiteInfo))
summary(factor(y_predJoin$SiteID))

#Assign values for mapping, 0 = true negative, 1 = true positive; 2 = false negative; 3 = false positive
y_predJoin$outcomeClass<-NA
y_predJoin$outcomeClass[y_predJoin$Obsclass== 0 & y_predJoin$PredClass==0]<-0
y_predJoin$outcomeClass[y_predJoin$Obsclass== 1 & y_predJoin$PredClass==1]<-1
y_predJoin$outcomeClass[y_predJoin$Obsclass== 1 & y_predJoin$PredClass==0]<-2
y_predJoin$outcomeClass[y_predJoin$Obsclass== 0 & y_predJoin$PredClass==1]<-3
summary(factor(y_predJoin$outcomeClass))
summary(factor(y_predJoin$outcomeClass))


#Write to file for us in GIS
#write.csv(y_predJoin, file="20250221_5ugL_rf_testDataForMapping_V3.csv")

#Use cutpoint to identify threshold for As 'detection' balancing sensitivity and specificity using Youden metric
library(cutpointr)
cp <- cutpointr(y_predJoin, ProbExceed, Obsclass, 
                method = maximize_metric, metric = youden, pot_class = 1)
summary(cp) #make note of the cutpoint value for comparison with lines 91-93 above
plot(cp)

# Predicting the Test set results 
y_predAdj <- data.frame(predict(model, newdata = AsTest, type="prob"))
y_predAdj$class<-0
y_predAdj$class[y_predAdj$X1>=0.396]<-1

# Confusion Matrix 
confusion_mtx <- confusionMatrix(factor(y_predAdj$class), factor(AsTest$ClassLTE5), positive ="1")
confusion_mtx

#Extract ROC Curve data for plotting
a<-as.data.frame(cp$roc_curve)
a$sens<-a$tp/(a$tp+a$fn) #sensitivity
a$spec<-a$tn/(a$tn+a$fp) #specificity
a$j<-(a$tp/(a$tp+a$fn))+(a$tn/(a$tn+a$fp))-1 #j-index, also called Youden value

##Make a plot like USGS PFAS paper S8
library(tidyverse)
df <- a %>%
  select(x.sorted, j, sens, spec) %>%
  gather(key = "variable", value = "value", -x.sorted)

ggplot(df, aes(x = x.sorted, y = value)) + 
  geom_line(aes(color = variable, linetype = variable)) + 
  scale_color_manual(values = c("black","darkred", "steelblue")) +
  xlab("As Detection Threshold - value above this threshold is considered a detection") + ylab("Metric Estimate")



#Spatial predictions
#method indicates these variables(field indexes): 25, 2, 3, 105, 1, 104, 9, 97, 36, 58
# A_Calcite, pH, prismy30yr, C_Tot_K_fs, Fe, C_Tot_14A, Top5_Ca, C_Hematite, A_Kaolinit, A_Tot_Flds

#Load raster files for prediction model
wd <- ("/Users/hoover/desktop/")
#wd <- ("/Users/aaronnuanez/desktop/")
rasterlist2 <-  list.files(paste0(wd,"spatialPredFormattedTifs"), full.names=TRUE, pattern=".tif$")
rasterlist2

d<-"/Users/hoover/desktop/spatialPredFormattedTifs/"
#d<-"/Users/aaronnuanez/desktop/spatialPredFormattedTifs/"

library(raster)
library(sp)
library(sf)
library(terra)

#Load each raster to check extent and crop as needed
A_Calcite<-raster(paste(d,"A_Calcite.tif", sep=""))
pH<-raster(paste(d,"pH.tif", sep=""))
prism30yr<-raster(paste(d,"prism30yr.tif", sep=""))
C_Tot_K_fs<-raster(paste(d,"C_Tot_K_fs.tif", sep=""))
Fe<-raster(paste(d,"Fe.tif", sep=""))
C_Tot_14A<-raster(paste(d,"C_Tot_14A.tif", sep=""))
Top5_Ca<-raster(paste(d,"Top5_Ca.tif", sep=""))
C_Hematite<-raster(paste(d,"C_Hematite.tif", sep=""))
A_Kaolinit<-raster(paste(d,"A_Kaolinit.tif", sep=""))
A_Tot_Flds<-raster(paste(d,"A_Tot_Flds.tif", sep=""))


#Change names so they match the XGB model
A_Calcite@data@names<-"A_Calcite"
pH@data@names<-"pH"
prism30yr@data@names<-"prism30yr"
C_Tot_K_fs@data@names<-"C_Tot_K_fs"
Fe@data@names<-"Fe"
C_Tot_14A@data@names<-"C_Tot_14A"
Top5_Ca@data@names<-"Top5_Ca"
C_Hematite@data@names<-"C_Hematite"
A_Kaolinit@data@names<-"A_Kaolinit"
A_Tot_Flds@data@names<-"A_Tot_Flds"

# create raster stack and convert to a maxtrix so it works with predict function for XGB
rstack1 <- stack(A_Calcite, pH, prism30yr, C_Tot_K_fs, Fe, C_Tot_14A, Top5_Ca, C_Hematite, A_Kaolinit, A_Tot_Flds)
rstack2<-rasterToPoints(rstack1)

#Make spatial prediction
spatialPred <- as.data.frame(predict (model, rstack2[,-c(1,2)], type="prob"))
colnames(spatialPred)[2]<-"AsPredict"

rstack3<-as.data.frame(rstack2)
rstack3$AsPred<-spatialPred$AsPredict

#Convert to raster
<<<<<<< HEAD
r<-rasterFromXYZ(rstack3[,c(1,2,11)], res=c(500,500))
=======
r<-rasterFromXYZ(rstack3[,c(1,2,13)], res=c(500,500))
crs(r) <- "EPSG:5070"
projection(r)
>>>>>>> 76459b16812439ad6e7f523698b2762a5d84ea57

plot(r)

#Write to file
<<<<<<< HEAD
writeRaster(r, "/Users/aaronnuanez/Desktop/20250218_randomForest_probAs5ugL", format='GTiff')
=======
writeRaster(r, "/Users/hoover/Desktop/20250228_randomForest_probAs5ugL", format='GTiff')
>>>>>>> 76459b16812439ad6e7f523698b2762a5d84ea57
