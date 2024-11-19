#Combine NNwells and WQP data tables 

library(reshape2)
library(gtools)
library(dplyr)
library(tidyr)
library(ggcorrplot)
library(recipes)


setwd("/Volumes/HooverShare/Shared_Group_Data/20_projects/06_coPlateau_Rework/")

#Clean up the workspace
rm(list=ls())

#Load the data frames
WQP <- read.csv("./02_Data/Raw_Data/WQP_As_All.csv", na.strings = "NA")
#WQP <- read.csv("./02_Data/Raw_Data/WQP_As_All.csv")
NNWells <- read.csv("./02_Data/Raw_Data/NNWells_As_All.csv", na.strings = "NA")

#Row bind the two data frames
As_COPlat_Data<-smartbind(WQP, NNWells)

#Drop Top Silver and Pyrite -
As_COPlat_Data<- As_COPlat_Data%>%
  select(-AN_Top5_Ag_7_17) %>%
  select(-AN_C_Pyrite_7_19)

#Check the variance of each column and then remove
#which(apply(As_COPlat_Data2, 2, var) == 0 )    ##the 2 indicates the look at variance by column, var = variance
##identify columns with zero variance
apply(As_COPlat_Data[,-c(1, 5:7,176)], 2, var, na.rm=TRUE)  #doesn't look like any 0 variance fields lefg


#Check for highly correlated variables
data<-As_COPlat_Data[,-c(5:7, 176)]  #Create a data frame with only the variables we want for 
tmp<-cor(data, use = "pairwise.complete.obs", method="spearman")
hc = findCorrelation(tmp, cutoff=0.9, names = FALSE); hc #identify highly correlated fields
reduced_Data = data[,-c(hc)] #remove highly correlated fields

#Add categorical variables back to the reduced data frame
As_COPlat_Data<-merge(reduced_Data, As_COPlat_Data[,c(1,5:7, 176)], by="SiteID" )

#now turn categorical variables into 0/1 dummy variables
#US_L3NAME
unique_vals <- unique(As_COPlat_Data$US_L3NAME)
for (val in unique_vals) {
 col_name <- paste("US_L3NAME", gsub(" ", "_", gsub(",", "", val)), sep = "_")
 As_COPlat_Data[[col_name]] <- ifelse(As_COPlat_Data$US_L3NAME == val, 1, 0)
}
table(As_COPlat_Data$US_L3NAME)
table(As_COPlat_Data$US_L3NAME_Southern_Rockies)

#UNIT_NAME
unique_vals <- unique(As_COPlat_Data$UNIT_NAME)
for (val in unique_vals) {
 col_name <- paste("UNIT_NAME", gsub(" ", "_", gsub(",", "", val)), sep = "_")
 As_COPlat_Data[[col_name]] <- ifelse(As_COPlat_Data$UNIT_NAME == val, 1, 0)
}
table(As_COPlat_Data$UNIT_NAME)
table(As_COPlat_Data$UNIT_NAME_Water)

#GENERALIZE
unique_vals <- unique(As_COPlat_Data$GENERALIZE)
for (val in unique_vals) {
 col_name <- paste("GENERALIZE", gsub(" ", "_", gsub(",", "", val)), sep = "_")
 As_COPlat_Data[[col_name]] <- ifelse(As_COPlat_Data$GENERALIZE == val, 1, 0)
}
table(As_COPlat_Data$GENERALIZE)
table(As_COPlat_Data$GENERALIZE_Water)

#Categorize wells by As conc
As_COPlat_Data <- As_COPlat_Data %>%
  mutate(ClassLTE1 = ifelse(as.numeric(As) <= 1, 1, 2)) %>%
  mutate(ClassLTE2 = ifelse(as.numeric(As) <= 2, 1, 2)) %>%
  mutate(ClassLTE3 = ifelse(as.numeric(As) <= 3, 1, 2)) %>%
  mutate(ClassLTE5 = ifelse(as.numeric(As) <= 5, 1, 2)) %>%
  mutate(ClassLTE10 = ifelse(as.numeric(As) <= 10, 1, 2)) %>%
  mutate(ClassGT10 = ifelse(as.numeric(As) > 10, 1, 2)) 

#Drop 3 fields that were categorical variables with NA as the field

<<<<<<< Updated upstream
<<<<<<< HEAD
#ggcorrplot(corr, type = "lower")

#Exclude no variance fields and highly correlated fields
drop_variance <- step_zv( As_COPlat_Data2,
  recipe,
  role = NA,
  trained = FALSE,
  group = NULL,
  removals = NULL,
  skip = FALSE,
  id = rand_id("zv")
)
=======
##remove columns with zero variance
As_COPlat_Data3<-As_COPlat_Data2[-as.numeric(which(apply(As_COPlat_Data2[,-c(1, 5:7,178)], 2, var) == 0.0 ))]
=======
As_COPlat_Data <-As_COPlat_Data[,-c(115,129,146)]

#Deal with NAs
#row sum to identify the number of fields with NAs
rowSums(is.na(As_COPlat_Data))

#Drop NAs
As_COPlat_Data_final<-As_COPlat_Data %>%
  drop_na()

#Check result for NAs
summary(rowSums(is.na(As_COPlat_Data_final))) #should be 0 NAs

write.csv(As_COPlat_Data_final, file = "~/Desktop/All_As_Data.csv", row.names = FALSE)


#Extra stuff
>>>>>>> Stashed changes

#Exclude no variance fields and highly correlated fields
#As_COPlat_Data2 %>% select(where(function(x) var(x) != 0))
>>>>>>> 1183d826cd312a4a19e6ae542e861b32b27fd085

#Add dummy variables back in
#reduced_Data2<-merge(reduced_Data,As_COPlat_Data3[,c(178:227)], by = )

#Compute a Correlation Matrix
#corr<- round(cor(data.new, method="spearman", use = "pairwise.complete.obs"),2)
#ggcorrplot(corr, type = "lower", hc.order = TRUE)

#As_COPlat_Data<-As_COPlat_Data[-as.numeric(which(apply(As_COPlat_Data[,-c(1, 5:7,176)], 2, var, na.rm=TRUE) == 0.0 ))]   #looks like this drops 1 field for 0 variance
#as.numeric(which(apply(As_COPlat_Data[,-c(1, 5:7,176)], 2, var, na.rm=TRUE) == 0))
#which(apply(As_COPlat_Data[,-c(1, 5:7,176)], 2, var) == 0.0 )

<<<<<<< Updated upstream
write.csv(drop_variance, file = "~/Desktop/All_As_Data.csv", row.names = FALSE)
=======
>>>>>>> Stashed changes



