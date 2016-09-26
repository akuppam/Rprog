setwd("~/MSDOT_LRTP/Data/ATRI")

library(dplyr)
library(ggplot2)

initial <- read.csv("MSDOT_ExtractedTruckTrips_Draft 2.csv", header = TRUE)

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
write.csv(initialNew2, file = "~/MSDOT_LRTP/Data/ATRI/MS-ATRI-new.csv", append = FALSE, sep = " ")

# ******************************************************************

qplot(OriginLatitude, OriginLongitude, data=initialNew2)

#####################

# need to have a categorical var as the first var
# need to have two continous vars after the categorical var
interaction.plot(initialNew2$PointsStop, initialNew2$TripSpeed, initialNew2$TripLength)
plot.design(initialNew2)

#data(initialNew2)
#cor(initialNew2[,1:4])

initialNew3 <- data.frame(initial$TripLength, initial$TripTime, initial$TripSpeed, initial$PointsStop)
head(initialNew3)

pairs(initialNew3, colors(distinct = FALSE))
