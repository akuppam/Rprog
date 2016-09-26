setwd("~/MAG_EST-CV Survey/StreetLight/External_for_Client/External_for_Client")

library(plyr)
library(dplyr)
library(ggplot2)
library(mcsm)
library(descr)
library(lattice)
library(reshape)

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

#barplot(table(magSLnew2$Date))
#summary(magSLnew2$Date)
#barplot(table(magSLnew2$Time))
#summary(magSLnew2$Time)

# ************************************
# Write to a file, suppress row names

write.csv(magSLnew2, "magSLnew2.csv", row.names=FALSE)

# *********************************
# frequency tabulations
# *********************

#magtables <- table(magSLnew2$Date, magSLnew2$VehicleWeight)
#margin.table(magtables, 1)
#margin.table(magtables, 2)

#prop.table(magtables)
#prop.table(magtables, 1)
#prop.table(magtables, 2)

#prop.table(mytable) # cell percentages
#prop.table(mytable, 1) # row percentages 
#prop.table(mytable, 2) # column percentages 

#magtables1 <- table(magSLnew2$VehicleID, magSLnew2$VehicleWeight)
#margin.table(magtables1, 1)
#margin.table(magtables1, 2)

#prop.table(magtables1)
#prop.table(magtables1, 1)
#prop.table(magtables1, 2)

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

#par(mfrow=c(2,2))
#barplot(table(magSLnew2SP$Date), main="Starting Point by Date")
#barplot(table(magSLnew2SP$Time), main="Starting Point by Time")
#barplot(table(magSLnew2EP$Date), main="Ending Point by Date")
#barplot(table(magSLnew2EP$Time), main="Ending Point by Time")

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

#par(mfrow=c(2,2))
#barplot(table(magSLnew3$TravelTime), main="Travel Time Distribution - Histogram")
#lines(table(magSLnew3$TravelTime), main="Travel Time Distribution - Lines")


#magSLnew3$DateTime.x <- strptime(magSLnew3$DateTime.x, "%m/%d/%y   %H:%M:%S %p")
#magSLnew3$DateTime.y <- strptime(magSLnew3$DateTime.y, "%m/%d/%y   %H:%M:%S %p")
#magSLnew3$DateTime.y - magSLnew3$DateTime.x
#summary(magSLnew3$DateTime.x)

#########
summary(magSLnew3)

write.csv(magSLnew3, "magSLnew3.csv", row.names=FALSE)

# #####################
#par(mfrow=c(2,2))
#plot(subset(magSLnew3,VehicleWeight.x == "Light", select = c(TripID, TravelTime)), main="Travel Time Distribution - Light")
#plot(subset(magSLnew3,VehicleWeight.x == "Mediu", select = c(TripID, TravelTime)), main="Travel Time Distribution - Medium")

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

#names(magSLnew3)

save(magSLnew3, file = "magSLnew3.Rda")
setwd("~/MAG_EST-CV Survey/StreetLight/External_for_Client/External_for_Client")
load("magSLnew3.Rda")
summary(magSLnew3)

# 3-Way Frequency Table 
#mytable <- table(A, B, C) 
#ftable(mytable) 

ctable <- table(magSLnew3$VehicleID.x, magSLnew3$Date.x)
write.csv(ctable, "ctable.csv", row.names=FALSE)
write.csv(ctable, "ctable1.csv", row.names=TRUE)

ctable2 <- table(magSLnew3$VehicleID.x, magSLnew3$VehicleWeight.x, magSLnew3$Date.x)
write.csv(ctable2, "ctable2.csv", row.names=TRUE)

# ************************
# analyze ctable2.csv
# make plots
# take it to tableau
# ***********************

ctable2 <- read.csv("ctable2.csv", header = TRUE)
head(ctable2)
summary(ctable2)

ctable3 <- table(magSLnew3$VehicleWeight.x, magSLnew3$Date.x)
write.csv(ctable3, "ctable3.csv", row.names=TRUE)

# ***************
# **********  travel duration  ***********************
setwd("~/MAG_EST-CV Survey/StreetLight/External_for_Client/External_for_Client")
rm(list = ls());options("scipen" = 10)
library(dplyr)
df1<-read.csv("magSLnew3.csv",h=T,stringsAsFactors = F)
summary(df1)
head(df1)
str(df1)  # 'df1' IS THE MASTER FILE

d1=as.POSIXct(df1$Time.x,format="%Y-%m-%d %H:%M:%S")
d1=as.numeric(d1);print(summary(d1))

d2=as.POSIXct(df1$Time.y,format="%Y-%m-%d %H:%M:%S")
d2=as.numeric(d2);print(summary(d2))

df2<-cbind(df1$TripID,df1$VehicleID.x,df1$VehicleID.y,d1,d2)
colnames(df2)=c("TripID","VehicleID_x","VehicleID_y","time_x","time_y")
df3=as.data.frame.matrix(df2)
df3$travel_time=(df3$time_y-df3$time_x)/60 ##computes the travel time by subtracting time.y-time.x

##This part of the code computes the variable as shown in the picture shared.
tt_xy=integer(length=length(d1)) #create a new integer vector/column with n-1 length. In this case 99.
for(i in 2:length(tt_xy)){tt_xy[i]=(d1[i]-d2[i-1])/60}#For loop computes the logic of c21-c12 etc.

##This dataframe contains the travel time and d3 variable computed.
df4=cbind(df3,tt_xy);print(head(df4,5))
write.csv(df4,file="magSLnew3_processed.csv",row.names = FALSE)
save(df4, file = "df4.Rda")

# **********  travel duration  ***********************
# **********  travel duration  ***********************
#
# **   use ggplot2 snippets ****************
#
# ******************************************

setwd("~/MAG_EST-CV Survey/StreetLight/External_for_Client/External_for_Client")
load("df4.Rda")
head(df4)

ggplot(df4, aes(x=travel_time, y=tt_xy)) + 
  geom_bar(stat="identity")

ggplot(df4, aes(x=travel_time, y=tt_xy)) + 
  geom_line()
  
# requires package ggvis
library(ggvis)
df4 %>% 
  ggvis(x = ~travel_time, fill := "gray") %>% 
  layer_histograms(width =  input_slider(min=mysliderMinValue, max=mysliderMax, value= mysliderStartingValue, label = "Bin Width")) 

# *******************************
# REPLACE -VE VALUES WITH ZEROS
# ZERO TRAVEL DURATION (tt_xy1) MEANS END OF TOUR
#********************************

setwd("~/MAG_EST-CV Survey/StreetLight/External_for_Client/External_for_Client")
load("df4.Rda")
summary(df4)
str(df4)

for (i in nrow(df4$tt_xy))
{
  if (df4$tt_xy[i]<0) {df4$tt_xy[i] = 0}
  else{df4$tt_xy[i] = df4$tt_xy[i]}
}

df4$tt_xy1 <- ifelse(df4$tt_xy < 0, 0, df4$tt_xy)

summary(df4)
str(df4)
head(df4, 10)

barplot(table(df4$tt_xy1))

write.csv(df4,file="magSLnew4_processed.csv",row.names = FALSE)
save(df4, file = "df4.Rda")

#***************
setwd("~/MAG_EST-CV Survey/StreetLight/External_for_Client/External_for_Client")
load("df4.Rda")
summary(df4)

plot(df4$tt_xy1, type = "o", col = "blue")

# *****************************
# COMBINE MASTER FILE WITH TRAVEL TIME AND TRAVEL DURATION
# *****************************
df5 <- cbind(df1, df4)
summary(df5)
str(df5)
write.csv(df5,file="magSLnew5_combined.csv",row.names = FALSE)
save(df5, file = "df5.Rda")

#mean(testvec[testvec != 0]) 

par(mfrow=c(2,2))
#densityplot(~tt_xy1, df5)
densityplot(~tt_xy1[tt_xy1!=0], df5)
#histogram(~tt_xy1, df5)
histogram(~tt_xy1[tt_xy1!=0], df5)

summary(df5$tt_xy1)
summary(df5$tt_xy1[df5$tt_xy1!=0])
summary(df5$travel_time)
summary(df5$travel_time[df5$travel_time>0])

densityplot(~tt_xy1[tt_xy1!=0] | VehicleWeight.x, df5)
histogram(~tt_xy1[tt_xy1!=0] | VehicleWeight.x, df5)
bwplot(~tt_xy1[tt_xy1!=0] | VehicleWeight.x, df5)

xyplot((travel_time[travel_time>0]) ~ (tt_xy1[tt_xy1!=0]) | VehicleWeight.x, df5)

# ***********************
# ***********************
# PIVOT TABLES
# LIGHT AND MEDIUM AVG TT AND DURATION
# ***********************
# ***********************
df5[1] <- NULL
str(df5)
write.csv(df5,file="magSLnew5_combined.csv",row.names = FALSE)
save(df5, file = "df5.Rda")

# ****************

setwd("~/MAG_EST-CV Survey/StreetLight/External_for_Client/External_for_Client")
load("df5.Rda")
summary(df5)
str(df5)

# ****************

lt = filter(df5, VehicleWeight.x %in% c("Light"))
lt = group_by(lt, VehicleWeight.x)
lt = summarise(lt, mean(travel_time[travel_time>0]), mean(tt_xy1[tt_xy1!=0]))
lt

md = filter(df5, VehicleWeight.x %in% c("Mediu"))
md = group_by(md, VehicleWeight.x)
md = summarise(md, mean(travel_time[travel_time>0]), mean(tt_xy1[tt_xy1!=0]))
md

# ****************
# ggplot2-Book09hWickham.pdf
# Figure 3.6, page 34
# ****************

df5$travel_time1 <- ifelse(df5$travel_time < 0, 0, df5$travel_time)
summary(df5)

write.csv(df5,file="magSLnew5_combined.csv",row.names = FALSE)
save(df5, file = "df5.Rda")

# ****************

setwd("~/MAG_EST-CV Survey/StreetLight/External_for_Client/External_for_Client")
load("df5.Rda")
summary(df5)
str(df5)

# ****************

qplot(travel_time1, tt_xy1, data = df5, facets = . ~ VehicleWeight.x) + geom_smooth()
qplot(travel_time1, tt_xy1, data = df5, facets = . ~ VehicleWeight.x)
qplot(travel_time1, tt_xy1, data = df5, facets = . ~ VehicleWeight.x) + geom_smooth(span=0.3)
qplot(travel_time1, tt_xy1, data = df5, facets = . ~ VehicleWeight.x) + geom_smooth(method = "lm", se = FALSE)

# ****************
# heatmap
# ****************
heatmap = ggplot(cntByDay, aes(x = DayTime, y = CntTimeTxt, fill = value, color = NULL))+ 
  geom_bin2d()+xlab("Day and Direction") + ylab("Time of Day") +  
  ggtitle("Average Weekday Count") + 
  scale_fill_gradient(low="green", high="red") + 
  scale_y_discrete(limits = rev(levels(cntByDay$CntTimeTxt))) 

ggplot(df5, aes(x = df5$travel_time1, y = df5$tt_xy1, fill = value, color = NULL))+ 
  geom_bin2d()+xlab("Date and Time of Day") + ylab("Stop Duration") +  
  ggtitle("Stop Duration by Date/TOD") + 
  scale_fill_gradient(low="green", high="red") + 
  scale_y_discrete(limits = rev(levels(df5$DateTime.x))) 

# ggplot same as qplot 
ggplot(df5, aes(travel_time1, tt_xy1, fill=value, color = NULL))
# OR
qplot(travel_time1, tt_xy1, data = df5)
qplot(DateTime.x, tt_xy1, data = df5)
qplot(Date.x, tt_xy1, data = df5)

# ************************************
# Below - 3 different stages of a plot
# DATE X STOP DURATION
# ************************************
qplot(Date.x, tt_xy1, data = df5)+
  geom_bin2d()+xlab("Date")+ylab("Stop Duration") +  
  ggtitle("Stop Duration by Date")
  
qplot(Date.x, tt_xy1, data = df5)+
  geom_bin2d()+xlab("Date")+ylab("Stop Duration") +  
  ggtitle("Stop Duration by Date")+
  scale_fill_gradient(low="green", high="red")  

qplot(Date.x, tt_xy1, data = df5)+
  geom_bin2d()+xlab("Date")+ylab("Stop Duration") +  
  ggtitle("Stop Duration by Date")+
  scale_fill_gradient(low="green", high="red")+  
  scale_y_discrete(limits = rev(levels(df5$Date.x))) 

# ************************************
# Below - 3 different stages of a plot
# DATE X TRAVEL TIME
# ************************************
qplot(Date.x, tt_xy1, data = df5)+
  geom_bin2d()+xlab("Date")+ylab("Stop Duration") +  
  ggtitle("Stop Duration by Date")

qplot(Date.x, tt_xy1, data = df5)+
  geom_bin2d()+xlab("Date")+ylab("Stop Duration") +  
  ggtitle("Stop Duration by Date")+
  scale_fill_gradient(low="green", high="red")  

qplot(Date.x, tt_xy1, data = df5)+
  geom_bin2d()+xlab("Date")+ylab("Stop Duration") +  
  ggtitle("Stop Duration by Date")+
  scale_fill_gradient(low="green", high="red")+  
  scale_y_discrete(limits = rev(levels(df5$Date.x))) 

qplot(Date.x, tt_xy1, data = df5)+
  geom_bin2d()+xlab("Date")+ylab("Stop Duration") +  
  ggtitle("Stop Duration by Date")+
  scale_fill_gradient(low="green", high="red")+  
  scale_y_discrete(limits = rev(levels(df5$VehicleWeight.x)))

# ************************************
# Below - 3 different stages of a plot
# VEHICLE TYPE X STOP DURATION
# ************************************
qplot(VehicleWeight.x, tt_xy1, data = df5)+
  geom_bin2d()+xlab("Vehicle Type")+ylab("Stop Duration") +  
  ggtitle("Stop Duration by Date")
jpeg(filename = "~/MAG_EST-CV Survey/StreetLight/External_for_Client/External_for_Client/plot1.jpeg")
plot(x,y)
dev.off()

qplot(VehicleWeight.x, tt_xy1, data = df5)+
  geom_bin2d()+xlab("Vehicle Type")+ylab("Stop Duration") +  
  ggtitle("Stop Duration by Date")+
  scale_fill_gradient(low="green", high="red")  
jpeg(filename = "~/MAG_EST-CV Survey/StreetLight/External_for_Client/External_for_Client/plot2.jpeg")
plot(x,y)
dev.off()

qplot(VehicleWeight.x, tt_xy1, data = df5)+
  geom_bin2d()+xlab("Vehicle Type")+ylab("Stop Duration") +  
  ggtitle("Stop Duration by Date")+
  scale_fill_gradient(low="green", high="red")+  
  scale_y_discrete(limits = rev(levels(df5$VehicleWeight.x))) 
jpeg(filename = "~/MAG_EST-CV Survey/StreetLight/External_for_Client/External_for_Client/plot3.jpeg")
plot(x,y)
dev.off()

# ************************************
# Below - 3 different stages of a plot
# VEHICLE TYPE X TRAVEL TIME
# ************************************
qplot(x=Date.x, y=travel_time1, data = df5)+
  geom_bin2d()+xlab("Date")+ylab("Travel Time") +  
  ggtitle("Travel time by Date")
jpeg(filename = "~/MAG_EST-CV Survey/StreetLight/External_for_Client/External_for_Client/plot4.jpeg")
plot(x,y)
dev.off()

qplot(Date.x, travel_time1, data = df5)+
  geom_bin2d()+xlab("Date")+ylab("Travel Time") +  
  ggtitle("Travel time by Date")+
  scale_fill_gradient(low="green", high="red")  
jpeg(filename = "~/MAG_EST-CV Survey/StreetLight/External_for_Client/External_for_Client/plot5.jpeg")
plot(x,y)
dev.off()

qplot(Date.x, travel_time1, data = df5)+
  geom_bin2d()+xlab("Date")+ylab("Travel Time") +  
  ggtitle("Travel time by Date")+
  scale_fill_gradient(low="green", high="red")+  
  scale_y_discrete(limits = rev(levels(df5$Date.x))) 
jpeg(filename = "~/MAG_EST-CV Survey/StreetLight/External_for_Client/External_for_Client/plot6.jpeg")
plot(x,y)
dev.off()
# ***************
# categorize the DateTime.x variable into finite groups
# if not, the catehgories will be way too mnay to handle
# and won't make sense
# ****************'

qplot(x=VehicleWeight.x, y=Time.x, data = df5)+
  geom_bin2d()+xlab("Vehicle Type")+ylab("Time of Day") +  
  ggtitle("TOD by Vehicle Type")+
  scale_fill_gradient(low="green", high="red")+
  scale_y_discrete(limits = rev(levels(df5$VehicleWeight.x)))+
  ggsave(filename = "~/MAG_EST-CV Survey/StreetLight/External_for_Client/External_for_Client/testplot.jpeg", device = "jpg")

# ********************
library(openxlsx) 
library(ggplot2) 
library(reshape2) 
library(plyr) 

smpData = read.xlsx("~/MAG_EST-CV Survey/StreetLight/External_for_Client/External_for_Client/magSLnew2-SAMPLE.xlsx", "magSLnew5_combined")
summary(smpData)
str(smpData)
t1=as.POSIXct(smpData$Time.x,format="%m/%d/%y %H:%M:%S")
smpData$t1 <- as.POSIXct(smpData$Time.x, origin="%m/%d/%y %H:%M:%S")
str(smpData)

smpData$t1=as.numeric(t1);print(summary(t1))

qplot(x=VehicleWeight.x, y=travel_time1, data = smpData)+
  geom_bin2d()+xlab("Vehicle Type")+ylab("Stop duration") +  
  ggtitle("TOD by Vehicle Type")+
  scale_fill_gradient(low="green", high="red")
#  scale_y_discrete(limits = rev(levels(smpData$VehicleWeight.x)))
#  ggsave(filename = "~/MAG_EST-CV Survey/StreetLight/External_for_Client/External_for_Client/testplot.jpeg", device = "jpg")

# **************************
# R pirate plots
# *************************
library("devtools")
library("yarrr")

setwd("~/MAG_EST-CV Survey/StreetLight/External_for_Client/External_for_Client")
load("df5.Rda")
summary(df5)
str(df5)

pirateplot(formula = travel_time1 ~ VehicleWeight.x,
           data = df5[which(df5$travel_time1>0),],
           line.fun = mean,
           line.lwd = 8,
           gl.col = gray(.9),
           bar.border.col = gray(.9),
           xlab = "Vehicle Type",
           ylab = "Travel Time",
           main = "Pirate Plot - Veh x TT")

#df[which(df$prop>0),]

pirateplot(formula = tt_xy1 ~ VehicleWeight.x,
           data = df5[which(df5$tt_xy1>0),],
           line.fun = mean,
           line.lwd = 8,
           gl.col = gray(.9),
           bar.border.col = gray(.9),
           xlab = "Vehicle Type",
           ylab = "Stop Duration",
           main = "Pirate Plot - Veh x Dur")

pirateplot(formula = tt_xy1 ~ Date.x,
           data = df5[which(df5$tt_xy1>0),],
           line.fun = mean,
           line.lwd = 8,
           gl.col = gray(.9),
           bar.border.col = gray(.9),
           xlab = "Date",
           ylab = "Stop Duration",
           main = "Pirate Plot - Veh x Dur")

pirateplot(formula = Date.x ~ tt_xy1,
           data = df5[which(df5$tt_xy1>0),],
           line.lwd = 8,
           gl.col = gray(.9),
           bar.border.col = gray(.9),
           xlab = "Date",
           ylab = "Stop Duration",
           main = "Pirate Plot - Veh x Dur")
