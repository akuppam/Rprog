library(dplyr)
library(ggplot2)
library(hflights)

#row.names(hflights) 							# writing row names to the console (also counting no of rows)
colnames(hflights) 								# writing variable or field names to the console
glimpse(hflights)

summary(hflights)
#sum(initial$duration, na.rm=TRUE) 				# summing "var 1" and removing missing values
#mean(initial$duration, na.rm=TRUE) 				# summing "var 1" and removing missing values
#max(initial$duration, na.rm=TRUE)					# write the max value from the "var 1" column
#min(initial$duration, na.rm=TRUE)					# write the min value from the "var 1" column

#qplot(x, y, data=initial, colour=stop_date)
# can't do both color and scale in one
#qplot(x, y, data=initial, colour=stop_date, alpha = I(1/100))
#qplot(x, y, data=initial, alpha = I(1/100))

qplot(Month, DepDelay, data=hflights, geom = "line", colour=UniqueCarrier)
qplot(Month, DepDelay, data=hflights, geom = "boxplot", colour=UniqueCarrier)

qplot(Month, DepTime, data=hflights, geom = "line", colour=UniqueCarrier)
qplot(Month, DepTime, data=hflights, geom = "boxplot", colour=UniqueCarrier)

# filter out select values of a variable

hflights2 <- filter(hflights, UniqueCarrier=="AA")
glimpse(hflights2)

qplot(Month, DepDelay, data=hflights2, colour=UniqueCarrier)

qplot(UniqueCarrier, DepDelay, data=hflights2, colour=UniqueCarrier)
#qplot(LU, new_dur_mins, data=initial, geom = "jitter", colour=LU)
#qplot(LU, new_dur_mins, data=initial, geom = "boxplot", colour=LU)
#qplot(start_time, LU, data=initial2, geom = "line", colour=LU)
