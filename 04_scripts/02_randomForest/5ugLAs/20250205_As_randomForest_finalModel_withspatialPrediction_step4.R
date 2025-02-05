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
train <- Asdata[Asdata$trainClassLTE10_splt == TRUE, ] #Need up update this field and dataframe to match what is produce in lines 21-24
test <- Asdata[Asdata$trainClassLTE10_splt == FALSE, ] #Need up update this field and dataframe to match what is produce in lines 21-24

#Make SiteID the row name so we can drop that field
rownames(train)<-train$SiteID
rownames(test)<-test$SiteID

#Drop unused fields
AsTrain<-train[,-c(1, 4, 109:112, 157:160, 162:168)] #Drop the As concentration, and the categorical variables we already transformed
AsTest<-test[,-c(1, 4, 109:112, 157:160, 162:168)]

AsTrain_y<-AsTrain[,151]
AsTest_y<-AsTest[,151]

#method indicates these variables(field indexes): 2, 27, 97, 8, 13, 25
#pH, A_Cs, C_Hematite, Top5_Be, Top5_Ni, A_Calcite

#Keep only useful precitors from variable selection step
AsTrain<-AsTrain[,c(2, 27, 97, 8, 13, 25, 151)] #Drop the As concentration, and the categorical variables we already transformed
AsTest<-AsTest[,c(2, 27, 97, 8, 13, 25, 151)]

#Ensure ClassLTE1 is a Factor (Categorical Variable)
AsTrain$ClassLTE10 <- as.factor(AsTrain$ClassLTE10)
AsTest$ClassLTE10  <- as.factor(AsTest$ClassLTE10)

#Run random forest model
#mtry is from step 1, might want to try different number of trees too
model<-randomForest(data=AsTrain, factor(ClassLTE10)~., mtry=2, ntree=500, importance = TRUE); 
print(model)

#Partial dependence plots to help us sort out the impact on an individual variable on As concentrations
partialPlot(model, AsTest, pH, "1", xlab="pH", ylab="As Class", lwd=4, col="green")
partialPlot(model, AsTest, A_Cs, "1", xlab="A_Cs", ylab="As Class", lwd=4, col="green")
partialPlot(model, AsTest, C_Hematite, "1", xlab="C_Hematite", ylab="As Class", lwd=4, col="green")
partialPlot(model, AsTest, Top5_Be, "1", xlab="Top5_Be", ylab="As Class", lwd=4, col="green")
partialPlot(model, AsTest, Top5_Ni, "1", xlab="Top5_Ni", ylab="As Class", lwd=4, col="green")
partialPlot(model, AsTest, A_Calcite, "1", xlab="A_Calcite", ylab="As Class", lwd=4, col="green")

# Predicting the Test set results 
y_pred <- predict(model, newdata = AsTest)

# Confusion Matrix 
confusion_mtx <- confusionMatrix(y_pred, factor(AsTest$ClassLTE10), positive ="1")
confusion_mtx


#Testing Data
rfpred <- data.frame(predict (model, AsTest, type="prob"))

#Adjust the "true" threshold using Youden value
#For a figure
y_predJoin<-data.frame(cbind(AsTest_y, rfpred))#change field to match outcome modeled, this applies to LT10

#rename fields for ease of use
colnames(y_predJoin)[1]<-"Obsclass"
colnames(y_predJoin)[2]<-"PredNotexceed"
colnames(y_predJoin)[3]<-"PredExceed"

#Use cutpoint to identify threshold for As 'detection' balancing sensitivity and specificity using Youden metric
library(cutpointr)
cp <- cutpointr(y_predJoin, PredExceed, Obsclass, 
                method = maximize_metric, metric = youden, pot_class = 1)
summary(cp) #make note of the cutpoint value for comparision with lines 91-93 above
plot(cp)

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
#method indicates these variables(field indexes): 2, 27, 97, 8, 13, 25
#pH, A_Cs, C_Hematite, Top5_Be, Top5_Ni, A_Calcite

#Load raster files for prediction model
wd <- ("/Users/hoover/desktop/")
rasterlist2 <-  list.files(paste0(wd,"spatialPredFormattedTifs"), full.names=TRUE, pattern=".tif$")
rasterlist2

d<-"/Users/hoover/desktop/spatialPredFormattedTifs/"

#library(terra)

#Load each raster to check extent and crop as needed
pH<-raster(paste(d,"pH.tif", sep=""))
A_Cs<-raster(paste(d,"A_Cs.tif", sep=""))
C_Hematite<-raster(paste(d,"C_Hematite.tif", sep=""))
Top5_Be<-raster(paste(d,"Top5_Be.tif", sep=""))
Top5_Ni<-raster(paste(d,"Top5_Ni.tif", sep=""))
A_Calcite<-raster(paste(d,"A_Calcite.tif", sep=""))

#Change names so they match the XGB model
pH@data@names<-"pH"
A_Cs@data@names<-"A_Cs"
C_Hematite@data@names<-"C_Hematite"
Top5_Be@data@names<-"Top5_Be"
Top5_Ni@data@names<-"Top5_Ni"
A_Calcite@data@names<-"A_Calcite"

# create raster stack and convert to a maxtrix so it works with predict function for XGB
rstack1 <- stack(pH, A_Cs, C_Hematite, Top5_Be, Top5_Ni, A_Calcite)
rstack2<-rasterToPoints(rstack1)

#Make spatial prediction
spatialPred <- as.data.frame(predict (model, rstack2[,-c(1,2)]))
colnames(spatialPred)[1]<-"AsPredict"

rstack3<-as.data.frame(rstack2)
rstack3$AsPred<-spatialPred$AsPredict

#Convert to raster
r<-rasterFromXYZ(rstack3[,c(1,2,8)], res=c(500,500))

#Make a plot and write to file
plot(r)

#Write to file
writeRaster(r, "20250130_randomForest_probAs10ugL", format='GTiff')
