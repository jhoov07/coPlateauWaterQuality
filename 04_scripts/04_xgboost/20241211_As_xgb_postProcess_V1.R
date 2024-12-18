library(caTools) 
#library(randomForest)
library(gbm)
library(xgboost) # for xgboost
library(caret)
library(tidyverse)
#library(SHAPforxgboost)

#install.packages("cutpointr") #install only once then comment out
library(cutpointr)

setwd("/Users/hoover/Documents/GitHub/coPlateauWaterQuality/03_data/")
#setwd("/Users/aaronnuanez/Documents/GitHub/coPlateauWaterQuality/03_data")

rm(list=ls())

#Load data
Asdata <- read.csv("All_As_Data.csv")

# Filter data into train and test sets based on logical variable
train <- Asdata[Asdata$trainClassLTE10_splt == TRUE, ] 
test <- Asdata[Asdata$trainClassLTE10_splt == FALSE, ] 

#Drop unused fields
trainAs<-train[,-c(1, 4, 109:112, 157:160, 162:168)] #Drop the As concentration, and the categorical variables we already transformed


testAs<-test[,-c(1, 4, 109:112, 157:160, 162:168)]



#Load XGB model
#Arsenic_xgb<-readRDS("./XGB_rds/2024-12-08_ClassLTE1_cv10_xgb.rds")
#Arsenic_xgb<-readRDS("./XGB_rds/2024-12-04_ClassLTE2_cv10_xgb.rds")
#Arsenic_xgb<-readRDS("./XGB_rds/2024-12-05_ClassLTE3_cv10_xgb.rds")
#Arsenic_xgb<-readRDS("./XGB_rds/2024-12-05_ClassLTE5_cv10_xgb.rds")
Arsenic_xgb<-readRDS("./XGB_rds/2024-12-08_ClassLTE10_cv10_xgb.rds")
Arsenic_xgb

#Make SiteID the row name so we can drop that field
rownames(train)<-train$SiteID

# Filter data into train and test sets based on logical variable 'trainCat2'
#train <- Asdata[Asdata$trainClassLTE2_splt == TRUE, ] #Need up update this field and dataframe to match what is produce in lines 21-24
#test <- Asdata[Asdata$trainClassLTE1_splt == FALSE, ] #Need up update this field and dataframe to match what is produce in lines 21-24
#test <- Asdata[Asdata$trainClassLTE2_splt == FALSE, ] #Need up update this field and dataframe to match what is produce in lines 21-24
#test <- Asdata[Asdata$trainClassLTE3_splt == FALSE, ] #Need up update this field and dataframe to match what is produce in lines 21-24
#test <- Asdata[Asdata$trainClassLTE5_splt == FALSE, ] #Need up update this field and dataframe to match what is produce in lines 21-24
test <- Asdata[Asdata$trainClassLTE10_splt == FALSE, ] #Need up update this field and dataframe to match what is produce in lines 21-24

#Make SiteID the row name so we can drop that field
#rownames(train)<-train$SiteID
rownames(test)<-test$SiteID

#Drop unused fields
#AsTrain<-train[,-c(1, 4, 109:112, 157, 159:168)] #Drop the As concentration, and the categorical variables we already transformed
#AsTest<-test[,-c(1, 4, 109:112, 157:160, 162:168)] #for use with LTE1
#AsTest<-test[,-c(1, 4, 109:112, 157,159:168)] #for use with LTE2
#AsTest<-test[,-c(1, 4, 109:112, 157:158, 160:168)] #for use with LTE3
#AsTest<-test[,-c(1, 4, 109:112, 157:159, 161:168)] #for use with LTE5
AsTest<-test[,-c(1, 4, 109:112, 157:160, 162:168)] #for use with LTE10


#Ensure ClassLTE2 is a Factor (Categorical Variable)
#AsTest$ClassLTE2 <- as.factor(AsTest$ClassLTE2)
#AsTest$ClassLTE3 <- as.factor(AsTest$ClassLTE3)
#AsTest$ClassLTE5 <- as.factor(AsTest$ClassLTE5)
AsTest$ClassLTE10  <- as.factor(AsTest$ClassLTE10)


# Predicting the Test set results 
#Predictions
y_pred = predict(Arsenic_xgb, newdata = test[,-1], type="prob") 
#y_pred = predict(Arsenic_xgb, newdata = test[,-1], missing ="NULL"); length(y_pred)

#Join probability with outcome from Test set
#y_predJoin<-cbind(test[,157], y_pred) #change field to match outcome modeled, this applies to LT1
#y_predJoin<-cbind(test[,158], y_pred) #change field to match outcome modeled, this applies to LT2
#y_predJoin<-cbind(test[,159], y_pred) #change field to match outcome modeled, this applies to LT3
#y_predJoin<-cbind(test[,160], y_pred) #change field to match outcome modeled, this applies to LT5
y_predJoin<-cbind(test[,161], y_pred) #change field to match outcome modeled, this applies to LT10

#rename fields for ease of use
colnames(y_predJoin)[1]<-"Obsclass"
colnames(y_predJoin)[2]<-"PrednoExceed"
colnames(y_predJoin)[3]<-"Predexceed"

#Use cutpoint to identify threshold for As 'detection' balancing sensitivity and specificity using Youden metric
cp <- cutpointr(y_predJoin, Predexceed, Obsclass, 
                method = maximize_metric, metric = youden)
summary(cp)
plot(cp)

#Extract ROC Curve data for plotting
a<-as.data.frame(cp$roc_curve)
a$sens<-a$tp/(a$tp+a$fn) #sensitivity
a$spec<-a$tn/(a$tn+a$fp) #specificity
a$j<-(a$tp/(a$tp+a$fn))+(a$tn/(a$tn+a$fp))-1 #j-index, also called Youden value

##Make a plot like USGS PFAS paper S8
df <- a %>%
  select(x.sorted, j, sens, spec) %>%
  gather(key = "variable", value = "value", -x.sorted)

ggplot(df, aes(x = x.sorted, y = value)) + 
  geom_line(aes(color = variable, linetype = variable)) + 
  scale_color_manual(values = c("black","darkred", "steelblue")) +
  xlab("As Detection Threshold - value above this threshold is considered a detection") + ylab("Metric Estimate")

#Assign predicted class using ROC threshold from previous steps
y_pred$class<-0
#y_pred$class[y_pred$'1' > 0.1258]<-1 #for LTE1
#y_pred$class[y_pred$'1' > 0.6034]<-1 #for LTE2
#y_pred$class[y_pred$'1' > 0.6023]<-1 #for LTE3
#y_pred$class[y_pred$'1' > 0.1405]<-1 #for LTE5
y_pred$class[y_pred$'1' > 0.1259]<-1 #for LTE10

#
# Confusion Matrix 
#confusion_mtx <- confusionMatrix(factor(y_pred$class), AsTest$ClassLTE1, positive = '1')
#confusion_mtx <- confusionMatrix(factor(y_pred$class), AsTest$ClassLTE2, positive = '1')
#confusion_mtx <- confusionMatrix(factor(y_pred$class), AsTest$ClassLTE3, positive = '1')
#confusion_mtx <- confusionMatrix(factor(y_pred$class), AsTest$ClassLTE5, positive = '1')
confusion_mtx <- confusionMatrix(factor(y_pred$class), AsTest$ClassLTE10, positive = '1', mode="sens_spec")
confusion_mtx

# Plotting model 
plot(Arsenic_xgb)

# Calculate Accuracy
accuracy <- confusion_mtx$overall['Accuracy']
accuracy

# Calculate kappa value
kappa_value <- confusion_mtx$overall['Kappa']
kappa_value

# Extract Sensitivity and Specificity for each class
sensitivity <- confusion_mtx$byClass[[1]]
sensitivity
specificity <- confusion_mtx$byClass[[2]]
specificity

# training data accuracy and kappa
# AvgAcc = accuracy  Avgkap = kappa
Arsenic_xgb$resample %>%
  arrange(Resample) %>%
  mutate(AvgAcc = mean(Accuracy)) %>%
  mutate(Avgkap = mean(Kappa))

ddd<-Arsenic_xgb$results
write.csv(ddd, file="20241208_As10_xgbAs_tuningOutput.csv")
max(Arsenic_xgb$results[[8]])

# Test data values
accuracy
kappa_value
sensitivity
specificity

importance <- varImp(Arsenic_xgb, scale = TRUE)

# Plot variable importance
plot(importance, top = 150, col = "blue",  main = "Variable Importance, XGB, As > 3ug/L")

