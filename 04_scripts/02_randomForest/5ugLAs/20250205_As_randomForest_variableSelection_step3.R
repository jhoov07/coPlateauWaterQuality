library(caTools) 
library(randomForest)
library(caret)
library(tidyverse)


#setwd("/Users/hoover/Documents/GitHub/coPlateauWaterQuality/03_data/")
setwd("/Users/aaronnuanez/Documents/GitHub/coPlateauWaterQuality/03_data")

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
rownames(train)<-train$SiteID
rownames(test)<-test$SiteID

#Drop unused fields
AsTrain<-train[,-c(1, 4, 109:112, 157:159, 161:168)] #Drop the As concentration, and the categorical variables we already transformed
AsTest<-test[,-c(1, 4, 109:112, 157:159, 161:168)]

AsTrain_x<-AsTrain[,-151]
AsTrain_y<-AsTrain[,151]

#Try variable tuning
library("VSURF")
toys.vsurf <- VSURF(x = AsTrain_x, y = AsTrain_y, mtry = 116, parallel = TRUE, ncores = 10, clusterType = "FORK")

summary(toys.vsurf)
plot(toys.vsurf)

toys.vsurf$varselect.interp

#method indicates these variables(field indexes): 25, 2, 3, 105, 1, 104, 9, 97, 36, 58
# A_Calcite, pH, prismy30yr, C_Tot_K_fs, Fe, C_Tot_14A, Top5_Ca, C_Hematite, A_Kaolinit, A_Tot_Flds

AsTrain<-AsTrain[,c(25, 2, 3, 105, 1, 104, 9, 97, 36, 58, 151)] #Drop the As concentration, and the categorical variables we already transformed
AsTest<-AsTest[,c(25, 2, 3, 105, 1, 104, 9, 97, 36, 58, 151)]

#Ensure ClassLTE5 is a Factor (Categorical Variable)
AsTrain$ClassLTE5 <- as.factor(AsTrain$ClassLTE5)
AsTest$ClassLTE5  <- as.factor(AsTest$ClassLTE5)

#Now tune mtry to determine best number with the reduced number of variables

# Fitting Random Forest to the train dataset 
tunegrid <- expand.grid(mtry = (1:(length(AsTrain)-1))) #Change to 1:7 if testing for real, 1:3 was used for model development

#this is the more accurate model out put 
#it was ran on the super computer
#classifier_RF = readRDS("/Users/aaronnuanez/Documents/GitHub/coPlateauWaterQuality/03_modelOutputs/01_randomForest/.rds")

# Fitting Random Forest to the train dataset 
classifier_RF<-train(
  data = AsTrain,
  factor(ClassLTE5) ~ ., 
  metric = "Accuracy",
  method = "rf",
  trControl = trainControl(method="cv", number = 5),    #change number = 10 if doing for real
  tuneGrid  = tunegrid,
  ntree = 500,
  verboseIter = TRUE  # Enable verbose output for troubleshooting
)

classifier_RF

#Run random forest model
#mtry is from step 1, might want to try different number of trees too
model<-randomForest(data=AsTrain, factor(ClassLTE5)~., mtry=3, ntree=500, importance = TRUE); 
print(model)

# Predicting the Test set results 
y_pred <- predict(model, newdata = AsTest)

# Confusion Matrix 
confusion_mtx <- confusionMatrix(y_pred, factor(AsTest$ClassLTE5), positive ="1")
confusion_mtx


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


#
varImpPlot(model, sort=T, n.var= 6, main= "Variable Importance", pch=16)
impt<-data.frame(model$importance)
print(impt[order(impt$MeanDecreaseGini, decreasing = TRUE), ]   )
