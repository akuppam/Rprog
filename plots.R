setwd("~/MAG_TruckTour/Data/ATRI/FINAL_STOPS")

library(dplyr)
library(ggplot2)

initial <- read.csv("FINAL_STOPS_v2.csv", header = TRUE)

#row.names(initial) 							# writing row names to the console (also counting no of rows)
colnames(initial) 								# writing variable or field names to the console

#summary(initial)
#sum(initial$duration, na.rm=TRUE) 				# summing "var 1" and removing missing values
#mean(initial$duration, na.rm=TRUE) 				# summing "var 1" and removing missing values
#max(initial$duration, na.rm=TRUE)					# write the max value from the "var 1" column
#min(initial$duration, na.rm=TRUE)					# write the min value from the "var 1" column

#qplot(x, y, data=initial, colour=stop_date)
# can't do both color and scale in one
#qplot(x, y, data=initial, colour=stop_date, alpha = I(1/100))
qplot(x, y, data=initial, alpha = I(1/100))
qplot(x, y, data=initial, colour=LU)
