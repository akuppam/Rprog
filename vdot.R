setwd("~/VDOT/ATRI")

library(dplyr)
library(ggplot2)

initial <- read.csv("RevisedVADOT_TruckTrips_Draft_5min_allTruckIDs.csv", header = TRUE)

#row.names(initial) 							# writing row names to the console (also counting no of rows)
colnames(initial) 								# writing variable or field names to the console

summary(initial)

# ******************************************************************
# separate OriginDtaeStamp (4/9/2014 16:48) into two new columns (4/9/2014) and (16:48)
# use strsplit() to split the column based on (" ") spaces in between the date and time
initial$OriginDateStamp <- as.character(initial$OriginDateStamp)
initialNew <- strsplit(initial$OriginDateStamp, " ")
colnames(initial)
colnames(initialNew)
head(initialNew)

# use do.call and rbind to append the two new variables into the main dataset 
initialNew2 <- data.frame(initial, do.call(rbind,initialNew))
colnames(initialNew2)
head(initialNew2)

# writew out the new data file into to the same directory
write.csv(initialNew2, file = "~/VDOT/ATRI/VA-ATRI-new.csv", append = FALSE, sep = " ")

# ******************************************************************