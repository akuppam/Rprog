setwd("~/Pythprog/Rprog") 

# reading csv file into 'initial' and indicating that the 1st row has variable or field names
initial <- read.csv("basics.csv", header=TRUE)

# Call both head() and summary() on basics.csv
# head() shows the top 5 rows
# tail() shows the last 5 rows
# summary() gives descrp. stats
# dim() gives no of obs. and no of vars.
head(initial)
tail(initial)
summary(initial)
dim(initial)
glimpse(initial)

#lut <- c("AA" = "100", "US" = "200", "UN" = "500", "SW" = "900",  "DL" = "800")
# ALWAYS MAKE SURE THE VALUES THAT WILL BE CONVERTED ARE IN ASCENDING ORDER FOR BOTH INTEGERS AND CHARACTERS
lut <- c("AA" = "A", "DL" = "E", "SW" = "D", "UN" = "C", "US" = "B")


# Use the lookup table to create a vector of code labels. Assign the vector to a variable
initial$var.3 <- lut[initial$var.3]

glimpse(initial)

# to display a concise table in the console that will fit
initial <- tbl_df(initial)
initial