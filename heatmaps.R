library(ggplot2)

#Reading in the data
chicagoMVT <- read.csv('motor_vehicle_theft.csv', stringsAsFactors = FALSE)

#Converting the date to a recognizable format
chicagoMVT$Date <- strptime(chicagoMVT$Date, format = '%m/%d/%Y %I:%M:%S %p')

#Getting the day and hour of each crime
chicagoMVT$Day <- weekdays(chicagoMVT$Date)
chicagoMVT$Hour <- chicagoMVT$Date$hour

#Sorting the weekdays
dailyCrimes <- as.data.frame(table(chicagoMVT$Day, chicagoMVT$Hour))
names(dailyCrimes) <- c('Day', 'Hour', 'Freq')
dailyCrimes$Hour <- as.numeric(as.character(dailyCrimes$Hour))
dailyCrimes$Day <- factor(dailyCrimes$Day, ordered = TRUE, 
                         levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))

#Plotting the number of crimes each day (line graph)
ggplot(dailyCrimes, aes(x = Hour, y = Freq)) + 
  geom_line(aes(group = Day, color = Day)) + xlab('Hour') + ylab('Number of thefts') + 
  ggtitle('Daily number of Motor Vehicle Thefts')


# ***********************
# http://www.siliconcreek.net/r/simple-heatmap-in-r-with-ggplot2
# **********************

library(openxlsx) 
library(ggplot2) 
library(reshape2) 
library(plyr) 


# Fix the path! 
cd = read.xlsx("countdata.xlsx", "Sheet1") 


# Summarize counts to get average weekdays 
cntByDay.1 = ddply(cd, .(DayOfWeek, CntTimeTxt), summarize, NB = mean(NB), SB = mean(SB)) 


# Reformat data to what ggplot2 wants 
cntByDay = melt(cntByDay.1, id.vars = c("DayOfWeek", "CntTimeTxt")) 


# We need some of these to be factors, and we need to reformat a little 
cntByDay$CntTimeTxt = factor(cntByDay$CntTimeTxt) 
cntByDay$DayTime = paste(cntByDay$DayOfWeek, cntByDay$variable, sep = " - ") 
cntByDay$DayTime = factor(cntByDay$DayTime, levels= c("Monday - NB", "Monday - SB", "Tuesday - NB", "Tuesday - SB", "Wednesday - NB", "Wednesday - SB", "Thursday - NB", "Thursday - SB", "Friday - NB", "Friday - SB", "Saturday - NB", "Saturday - SB", "Sunday - NB", "Sunday - SB")) 


# Plot the data 
heatmap = ggplot(cntByDay, aes(x = DayTime, y = CntTimeTxt, fill = value, color = NULL))+ 
  geom_bin2d()+xlab("Day and Direction") + ylab("Time of Day") +  
  ggtitle("Average Weekday Count") + 
  scale_fill_gradient(low="green", high="red") + 
  scale_y_discrete(limits = rev(levels(cntByDay$CntTimeTxt))) 

# **************************
# R pirate plots
# http://www.r-bloggers.com/the-pirate-plot-2-0-the-rdi-plotting-choice-of-r-pirates/
# *************************
install.packages("devtools")
library("devtools")
install_github("ndphillips/yarrr")
library("yarrr")

library(openxlsx) 

magSLdata = read.xlsx("~/MAG_EST-CV Survey/StreetLight/External_for_Client/External_for_Client/magSLsample.xlsx", "Sheet1")
summary(magSLdata)
str(magSLdata)
head(magSLdata)


pirateplot(formula = travel_time1 ~ VehicleWeight.x,
           data = magSLdata,
           line.fun = mean,
           line.lwd = 8,
           gl.col = gray(.9),
           bar.border.col = gray(.9),
           xlab = "Vehicle Type",
           ylab = "Travel Time",
           main = "Pirate Plot - Veh x TT")

