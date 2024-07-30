library(caret)
library(caTools)
library(ggplot2)
library(readr)
library(gbm)

#setwd("C:/Users/jhoover/Documents/GitHub/coPlateauWaterQuality/01_data")
setwd("/Users/austinmartinez/Documents/GitHub/coPlateauWaterQuality/01_data/CoPlateau_U")

rm(list=ls())

Udata = read.csv("Cleaned_U_GIS_Filtered.csv", na.strings = "NULL")

#elevation kepted being viewed as catagorical so made it numeric
Udata$Elevation <- as.numeric(Udata$Elevation)

#take out NAs
Udata <- Udata[complete.cases(Udata), ]

# Filter data into train and test sets based on logical variable 'spl3cat'
train <- Udata[Udata$spl5 == TRUE, ]
test <- Udata[Udata$spl5 == FALSE, ]

#Drop unused fields
UTrain<-train[,-c(1:5,208:210, 212:213)]
UTest<-test[,-c(1:5,208:210, 212:213)]


#Ensure As3Cat is a Factor (Categorical Variable)

UTrain$As3Cat <- as.factor(UTrain$As3Cat)
UTest$As3Cat <- as.factor(UTest$As3Cat)

#this is the more acurate model output 
#it was ran on the super computer
#Uranium_boost = readRDS("/Users/austinmartinez/Documents/GitHub/coPlateauWaterQuality/03_modelOutputs/02_boostedRegTrees/2024-07-25_as_brt.rds")

#this runs in 5 mims
Uranium_boost <- train(
  As3Cat ~ .,  # Specify the target variable as As3Cat
  data = UTrain,  
  method = "gbm", 
  trControl = trainControl(
    method = "cv",
    number = 2,
    verboseIter = TRUE  # Enable verbose output for troubleshooting
  ),
  tuneGrid = expand.grid(
    "n.trees" = seq(from = 1, to = 21, by = 5),  #from USGS paper, might want to scale down for our work here
    "interaction.depth" = seq(from = 2, to = 16, by = 4),  #adapted from USGS paper, might want to scale down for our work here
    "shrinkage" = seq(from =0.004, 0.008, by = 0.004),  #adapted from USGS paper, might want to scale down for our work here
    "n.minobsinnode" = 8) #from USGS paper, might want to scale down for our work here
)



# Check the results
summary(Uranium_boost)

#Evaluate the model on the test set
predictions <- predict(Uranium_boost, newdata = UTest)
conf_matrix = confusionMatrix(predictions, UTest$As3Cat)

# training data accuracy and kappa
# AvgAcc = accuracy  Avgkap = kappa
Uranium_boost$resample %>%
  arrange(Resample) %>%
  mutate (AvgAcc = mean(Accuracy)) %>%
  mutate (Avgkap = mean(Kappa))

# Extract sensitivity and specificity
sensitivity <- conf_matrix$byClass[, "Sensitivity"]
specificity <- conf_matrix$byClass[, "Specificity"]
accuracy <- conf_matrix$overall['Accuracy']
kappa_value <- conf_matrix$overall['Kappa']

# Test data values
accuracy
kappa_value
sensitivity
specificity

importance <- varImp(Uranium_boost, scale = FALSE)

# Plot variable importance
plot(importance, top = 10, col = "blue",  main = "Boosted Regression Tree")
