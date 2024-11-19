setwd("~/Desktop")
data = read.csv("nurewtr.csv")

# only keeps rows with "UT", "CO", "AZ", "NM" in state collum
data1 <- data[data$state %in% c("UT", "CO", "AZ", "NM"), ]

#removes all variables besides "welldpth", "latitude", "longitude", "u_dn_ppb", "u_fl_ppb", "state"
filtered_data <- data1[, c("rec_no","welldpth", "latitude", "longitude", "u_dn_ppb", "u_fl_ppb", "state")]

#write.csv(filtered_data, file = "~/Desktop/New_U_data.csv", row.names = FALSE)



# combining u_fl_ppb and u_dn_ppb

#shows how many NAs are in the data before cleaning
na_count_before <- sum(is.na(filtered_data$u_fl_ppb))
cat("Number of NAs in u_fl_ppb before replacement:", na_count_before, "\n")

#replace NA values in u_fl_ppb with corresponding values from u_dn_ppb
filtered_data$u_fl_ppb[is.na(filtered_data$u_fl_ppb)] <- filtered_data$u_dn_ppb[is.na(filtered_data$u_fl_ppb)]

#take absolute value of field so there are no negative values; negatives mean less than that concentration
filtered_data$u_fl_ppb<-abs(filtered_data$u_fl_ppb); summary(filtered_data$u_fl_ppb)

#shows how many NAs are in the data before cleaning after cleaning
na_count_after <- sum(is.na(filtered_data$u_fl_ppb))
cat("Number of NAs in u_fl_ppb after replacement:", na_count_after, "\n")

#I did this to check my work,
# makes dataframe with all rows with NAs in both u_fl_ppb and u_dn_ppb to check if it matched # of NAs
na_rows <- which(is.na(filtered_data$u_fl_ppb))
na_filtered_data <- filtered_data[is.na(filtered_data$u_fl_ppb), ]
table(na_filtered_data$u_fl_ppb)

write.csv(filtered_data, file = "~/Desktop/6New_U_data.csv", row.names = FALSE)
