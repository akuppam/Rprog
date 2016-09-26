setwd("~/MAG_EST-CV Survey/StreetLight/External_for_Client/External_for_Client")

library(plyr)
library(dplyr)
library(ggplot2)
library(mcsm)
library(descr)

magSL <- read.csv("trips-Tripends.csv", header = TRUE)

#row.names(initial) 							# writing row names to the console (also counting no of rows)
colnames(magSL) 								# writing variable or field names to the console

summary(magSL)

# ******************************************************************
# change variable names
# ****************************

#X <- data.frame(????)
colnames(magSL) <- c("VehicleID", "VehicleWeight", "TripID", "PointIndex", "PointFlag", "PointLatitude", "PointLongitude", "PointTime", "TripEnds")
colnames(magSL) 								# writing variable or field names to the console
summary(magSL)

# ******************
# converting UTC-epoch to date and time stamp
# *******************
# val1 <- 1352068320
# val2 <- 1427849178
#as.POSIXct(val1, origin="1970-01-01")
#as.POSIXct(val2, origin="1970-01-01")
# *******************
# if you only want DATE 
# *******************
#as.Date(as.POSIXct(val1, origin="1970-01-01"))
#as.Date(as.POSIXct(val2, origin="1970-01-01"))
# *******************

magSL$DateTime <- as.POSIXct(magSL$PointTime, origin="1970-01-01")
summary(magSL$DateTime)
summary(magSL)

# *******************************************
# separate OriginDtaeStamp (4/9/2014 16:48) into two new columns (4/9/2014) and (16:48)
# use strsplit() to split the column based on (" ") spaces in between the date and time
magSL$DateTime <- as.character(magSL$DateTime)
magSLnew <- strsplit(magSL$DateTime, " ")
colnames(magSLnew)
head(magSLnew)

# use do.call and rbind to append the two new variables into the main dataset 
magSLnew2 <- data.frame(magSL, do.call(rbind,magSLnew))
colnames(magSLnew2)
head(magSLnew2)

# *****************
colnames(magSLnew2) <- c("VehicleID", "VehicleWeight", "TripID", "PointIndex", "PointFlag", "PointLatitude", "PointLongitude", "PointTime", "TripEnds", "DateTime", "Date", "Time")
colnames(magSLnew2) 								# writing variable or field names to the console
summary(magSLnew2)

# *******************
# barplots showing freq distribution of string variables
# ******************

barplot(table(magSLnew2$Date))
summary(magSLnew2$Date)
barplot(table(magSLnew2$Time))
summary(magSLnew2$Time)

# ************************************
# Write to a file, suppress row names

write.csv(magSLnew2, "magSLnew2.csv", row.names=FALSE)

# *********************************
# frequency tabulations
# *********************

magtables <- table(magSLnew2$Date, magSLnew2$VehicleWeight)
margin.table(magtables, 1)
margin.table(magtables, 2)

prop.table(magtables)
prop.table(magtables, 1)
prop.table(magtables, 2)

prop.table(mytable) # cell percentages
prop.table(mytable, 1) # row percentages 
prop.table(mytable, 2) # column percentages 

magtables1 <- table(magSLnew2$VehicleID, magSLnew2$VehicleWeight)
margin.table(magtables1, 1)
margin.table(magtables1, 2)

prop.table(magtables1)
prop.table(magtables1, 1)
prop.table(magtables1, 2)

#ftable(magtables3way)
# LOOK INTO WRITE FREQ TABLES TO CSV

###########################
# ***  creating two data files, one for SP and one for EP ****** #
###########################
summary(magSLnew2)

magSLnew2SP <- subset(magSLnew2, magSLnew2$PointFlag == "SP")
summary(magSLnew2SP)

magSLnew2EP <- subset(magSLnew2, magSLnew2$PointFlag == "EP")
summary(magSLnew2EP)

# *******************
# barplots showing freq distribution of string variables
# ******************

par(mfrow=c(2,2))
barplot(table(magSLnew2SP$Date), main="Starting Point by Date")
barplot(table(magSLnew2SP$Time), main="Starting Point by Time")
barplot(table(magSLnew2EP$Date), main="Ending Point by Date")
barplot(table(magSLnew2EP$Time), main="Ending Point by Time")

magSLnew3 <- merge(magSLnew2SP, magSLnew2EP, by = "TripID")
summary(magSLnew3)
colnames(magSLnew3)

#R> df$time2 <- strptime(df$time2, "%Y-%m-%d %H:%M:%OS")
#R> df$time3 <- strptime(df$time3, "%Y-%m-%d %H:%M:%OS")
#R> df$time2 - df$time3
#strptime("1/22/2013 11:00:00 pm",format="%m/%d/%Y %I:%M:%S %p")
#[1] "2013-01-22 23:00:00"

#3/31/2015  5:25:22 PM
#5:25:22 PM

magSLnew3$Time.x <- strptime(magSLnew3$Time.x, "%H:%M:%S")
magSLnew3$Time.y <- strptime(magSLnew3$Time.y, "%H:%M:%S")
summary(magSLnew3$Time.x)
summary(magSLnew3$Time.y)

magSLnew3$TravelTime <- as.numeric(magSLnew3$Time.y - magSLnew3$Time.x)

summary(magSLnew3$TravelTime)

head(magSLnew3$TravelTime)

# ***********************************

par(mfrow=c(2,2))
barplot(table(magSLnew3$TravelTime), main="Travel Time Distribution - Histogram")
lines(table(magSLnew3$TravelTime), main="Travel Time Distribution - Lines")


#magSLnew3$DateTime.x <- strptime(magSLnew3$DateTime.x, "%m/%d/%y   %H:%M:%S %p")
#magSLnew3$DateTime.y <- strptime(magSLnew3$DateTime.y, "%m/%d/%y   %H:%M:%S %p")
#magSLnew3$DateTime.y - magSLnew3$DateTime.x
#summary(magSLnew3$DateTime.x)

#########
summary(magSLnew3)

write.csv(magSLnew3, "magSLnew3.csv", row.names=FALSE)

# #####################
par(mfrow=c(2,2))
plot(subset(magSLnew3,VehicleWeight.x == "Light", select = c(TripID, TravelTime)), main="Travel Time Distribution - Light")
plot(subset(magSLnew3,VehicleWeight.x == "Mediu", select = c(TripID, TravelTime)), main="Travel Time Distribution - Medium")

# ################
# CHECK THE DATA IN SPSS FOR THE -VE VALUES AND CORRECT THEM
# GET NETWORK DISTANCES 
# CHECK WITH PETYA IF SHE CAN ATTACH LU'S TO SL DATA
# ################

#tlfd <- cut(magSLnew3$TravelTime, breaks=seq(0,1000,5))
#summary(tlfd)
#write.csv(summary(tlfd), "tlfd.csv", row.names=FALSE)

#ddply(magSLnew3, .(xrange), summarize, mean_tt=mean(TravelTime))

# *************************
# 3-Way Frequency Table 
#mytable <- table(A, B, C) 
#ftable(mytable) 

# Frequency count
#crosstab(magSLnew3, row.vars = "VehicleID.x", col.vars = "Date.x", type = "f")

names(magSLnew3)
