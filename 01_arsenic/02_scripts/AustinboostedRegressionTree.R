library(caret)
library(caTools)
library(ggplot2)
library(readr)
library(gbm)

#setwd("C:/Users/jhoover/Documents/GitHub/coPlateauWaterQuality/01_data")
setwd("/Users/austinmartinez/Documents/GitHub/coPlateauWaterQuality/01_data")
Asdata = read.csv("AsModelInput.csv")

# Filter data into train and test sets based on logical variable 'spl3cat'
train <- Asdata[Asdata$spl5 == TRUE, ]
test <- Asdata[Asdata$spl5 == FALSE, ]

# Split the data into training (70%) and testing (30%) sets
#sample_indices <- sample(1:nrow(Asdata), 0.7 * nrow(Asdata))
#train_data <- Asdata[sample_indices, ]
#test_data <- Asdata[-sample_indices, ]

# Drop rows with NA values in columns 9 to 93 for test data
AsTrain <- train[complete.cases(train[, 9:93]), ]
AsTest <- test[complete.cases(test[, 9:93]), ]
Clean_As <- rbind(AsTrain ,AsTest)

#Ensure As3Cat is a Factor (Categorical Variable)
AsTrain$As3Cat <- as.factor(AsTrain$As3Cat)
AsTest$As3Cat <- as.factor(AsTest$As3Cat)
Clean_As$As3Cat <- as.factor(Clean_As$As3Cat)

# original boost
#Arsenic_boost <- train(
#  As3Cat ~ .,  # Specify the target variable as As3Cat
#  data = Clean_As[, -c(1:3, 5:8)],  
#  method = "gbm", 
#  trControl = trainControl(
#    method = "cv",
#    number = 5,
#    verboseIter = TRUE  # Enable verbose output for troubleshooting
#  ),
#  tuneGrid = expand.grid(
#    "n.trees" = seq(50, 200, by = 50),
#    "interaction.depth" = 1:3,
#    "shrinkage" = c(0.1, 0.01, 0.001),
#    "n.minobsinnode" = 5))

Arsenic_boost <- train(
  As3Cat ~ .,  # Specify the target variable as As3Cat
  data = Clean_As[, -c(1:3, 5:8)],  
  method = "gbm", 
  trControl = trainControl(
    method = "cv",
    number = 10,
    verboseIter = TRUE  # Enable verbose output for troubleshooting
  ),
  tuneGrid = expand.grid(
    "n.trees" = seq(from = 1000, to = 5000, by = 500),  #from USGS paper, might want to scale down for our work here
    "interaction.depth" = seq(from = 2, to = 16, by = 4),  #adapted from USGS paper, might want to scale down for our work here
    "shrinkage" = seq(from = 0.004, to = 0.012, by = 0.004),  #adapted from USGS paper, might want to scale down for our work here
    "n.minobsinnode" = 8) #from USGS paper, might want to scale down for our work here
)



# Check the results
summary(Arsenic_boost)

#Evaluate the model on the test set
predictions <- predict(Arsenic_boost, newdata = AsTest[, -(1:8)])
confusionMatrix(predictions, AsTest$As3Cat)

