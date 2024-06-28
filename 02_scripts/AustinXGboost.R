library(caTools) 
library(caret)
library(gbm)
library(xgboost) # for xgboost
library(tidyverse) # general utility functions

#setwd("C:/Users/jhoover/Documents/GitHub/coPlateauWaterQuality/01_data")
#setwd("C:/Users/austinmartinez/Documents/GitHub/coPlateauWaterQuality/01_data")

All_Asdata = read.csv("AsModelInput.csv")
# set a random seed & shuffle data frame
set.seed(1234)
Asdata <- All_Asdata[sample(1:nrow(All_Asdata)), ]

#turning all 1-0(true and false) to booleans
# List of non-numeric variables
non_numeric_vars <- c(
  "rt_carb", "rt_clast_c", "rt_clast_f", "rt_clast_u", "rt_meta", "rt_plut_qtz",
  "D3", "De", "DSe", "Kg", "O2", "Oe", "PP4", "Pzg1", "Pzg2", "Q", "Qv", "S2", "Se",
  "Te2", "Tm", "Tmc", "Toc", "Tp", "Tpc", "Tpv", "Tr", "Txc", "uK2", "uK3", "uK3b",
  "uK4", "uPz", "Wgn", "uc_12", "uc_221", "uc_312", "uc_321", "uc_322", "uc_331",
  "uc_421", "uc_422", "uc_431", "uc_451", "uc_620", "uc_811", "uc_812", "uc_821",
  "uc_822", "uc_910", "uc_920", "uc_940", "uc_960", "uc_970", "uc_999", "na_10.1",
  "na_11.1", "na_12.1", "na_13.1", "na_6.2", "na_7.1", "na_8.1", "na_8.2", "na_9.2",
  "na_9.3", "na_9.4", "na_9.5", "na_9.6"
)

#By using ~ . == 1, it ensures that each column's values are evaluated such that 1 becomes TRUE and 0 becomes FALSE
AsLabels <- Asdata %>%
  select(all_of(non_numeric_vars)) %>%
  mutate(across(everything(), ~ . == 1))

# Removes rows 1-8(unessecary data) and removes all nonnumeric vars because they aren't booleans
clean_Asdata <- Asdata %>%
  select(-c(1:8, all_of(non_numeric_vars)))

#combine AsLabels(booleaned nonnumeric vars) and the clean_Asdata set
FullcleanAs <- cbind(AsLabels, clean_Asdata)

# Ensure the result is a data frame
FullcleanAs <- as.data.frame(FullcleanAs)

# make it a metrix
As_matrix <- FullcleanAs %>%
  data.matrix()

# get the numb 70/30 training test split
numberOfTrainingSamples <- round(length(Asdata) * .7)

# As3Cat is the target variable
train_labels <- All_Asdata$As3Cat 
test_labels <- All_Asdata$As3Cat




#FEEL FREE TO COMMENT OUT. This model dosen't work wether or not its used)
# I'm making As3Cat into a true or false senerio.(they did this in the tutotial)
# C3=true(over the amount of safe As conc) C2,C1=False(under the Safe As conc)
# Create a binary label indicating if the concentration is in C3
#training
binary_labels <- ifelse(test_labels == "C3", 1, 0)
# Verify the transformation
table(binary_labels)
# Optionally, update test_labels to binary_labels if you want to use it directly
test_labels <- binary_labels
#testing
binary_labels <- ifelse(test_labels == "C3", 1, 0)
# Verify the transformation
table(binary_labels)
# Optionally, update test_labels to binary_labels if you want to use it directly
test_labels <- binary_labels




# training data
train_data <- As_matrix[1:numberOfTrainingSamples,]
train_labels <- train_labels[1:numberOfTrainingSamples]

# testing data
test_data <- As_matrix[-(1:numberOfTrainingSamples),]
test_labels <- test_labels[-(1:numberOfTrainingSamples)]

#take out NAs
train_data <- train_data[complete.cases(train_data)]
test_data <- test_data[complete.cases(test_data)]
train_labels <- train_labels[complete.cases(train_labels)]
test_labels <- test_labels[complete.cases(test_labels)]

# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)
#i get this error everytime
# > dtest <- xgb.DMatrix(data = test_data, label= test_labels)
#Error in xgb.DMatrix(data = test_data, label = test_labels) : 
  #xgb.DMatrix does not support construction from double
