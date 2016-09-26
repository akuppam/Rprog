library(dplyr)
library(ggplot2)
library(hflights)

flights <- tbl_df(hflights)

#row.names(hflights) 							# writing row names to the console (also counting no of rows)
colnames(flights) 								# writing variable or field names to the console
glimpse(flights)

summary(flights)
#sum(initial$duration, na.rm=TRUE) 				# summing "var 1" and removing missing values
#mean(initial$duration, na.rm=TRUE) 				# summing "var 1" and removing missing values
#max(initial$duration, na.rm=TRUE)					# write the max value from the "var 1" column
#min(initial$duration, na.rm=TRUE)					# write the min value from the "var 1" column

# filter out select values of a variable

#hflights2 <- filter(hflights, UniqueCarrier=="AA")
#glimpse(hflights2)

# does not store the new variable 'Speed'
flights %>%
  select(Distance, AirTime) %>%
  mutate(Speed = Distance/AirTime*60)
glimpse(flights)

# store the new variable 'Speed'
flights <- flights %>% mutate(Speed = Distance/AirTime*60)
glimpse(flights)

