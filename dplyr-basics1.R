# Load the dplyr package
library(dplyr)

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

# Create the object 'dur', containing only the new_dur_secs variable 
dur <- truckstops$new_dur_secs
