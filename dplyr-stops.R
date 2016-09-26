# Load the dplyr package
library(dplyr)
# Load the ggplot2 package
library (ggplot2)

# setting working directory or path
#setwd("~/Pythprog/Rprog") 
setwd("~/MAG_TruckTour/Data/ATRI/FINAL_STOPS")

# reading csv file into 'initial' and indicating that the 1st row has variable or field names
truckstops <- read.csv("FINAL_STOPS_v2.csv", header=TRUE)

# Call both head() and summary() on the *.csv file
# head() shows the top 5 rows
# tail() shows the last 5 rows
# summary() gives descrp. stats
# dim() gives no of obs. and no of vars.
head(truckstops)
tail(truckstops)
summary(truckstops)
dim(truckstops)

# to display a concise table in the console that will fit
truckstops <- tbl_df(truckstops)
truckstops

# to get a glimpse of all variables with a few values
glimpse(truckstops)

# Create the object 'dur', containing only the new_dur_mins variable 
dur <- truckstops$new_dur_mins
glimpse(truckstops)

# using 'group_by' for LU categorical variable and computing average duration spent at each LU using 'summarise'
# fyi, the new variable does NOT get stored - only for console display purposes.  
# Also, you can't store 'avgdur' as it is pivotting off of LU variable
# use 'na.rm = TRUE' to NOT include N/A or missing values in the computations
truckstops %>%
  group_by(LU) %>%
  summarise(avgdur = mean(new_dur_mins, na.rm=TRUE))
glimpse(truckstops)

# create a new variable and store it
truckstops <- truckstops %>% mutate(new_dur_hrs = new_dur_mins/60)
glimpse(truckstops)

truckstops %>%
  group_by(LU) %>%
  summarise(avgdur = mean(new_dur_hrs, na.rm=TRUE))

# cannot do a plot with a 'summarised' variable
#qplot(LU, avgdur, data = truckstops, colour=LU)

# summarise every variable
truckstops %>%
  group_by(LU) %>%
  summarise_each(funs(mean(new_dur_mins, na.rm=TRUE)))
glimpse(truckstops)

# use na.rm=TRUE for certain variables
truckstops %>%
  group_by(LU) %>%
  summarise((avgdurmins = mean(new_dur_mins, na.rm=TRUE)), (avgdurhrs = mean(new_dur_hrs, na.rm=TRUE)))

# count observations within each LU using an indicator variable
truckstops %>%
  group_by(LU) %>%
  summarise(stop_ind = n())

# count observations within each LU using an indicator variable
truckstops %>%
  group_by(stop_date) %>%
  summarise(stop_ind = n())

# randomly sample a fraction of rows, with replacement
truckstops %>% sample_frac(0.25, replace = TRUE)
truckstops %>% sample_frac(0.25)

# str (in R) is same as glimpse (in dplyr)
str(truckstops)
glimpse (truckstops)

# filtering ranges of values
truckstops %>% filter(between(start_time, 600, 900))
