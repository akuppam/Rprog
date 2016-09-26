setwd("~/Pythprog/Rprog")

library(dplyr)
library(ggplot2)

initial <- read.csv("month.csv", header = TRUE)

ts(data=initial, start=1, end=12, frequency=1)

ts(data=initial, start=1, end=12, frequency=2)

ts(data=initial, start=1, end=12, frequency=3)

ts(data=initial, start=1, end=6, frequency=1)

ts(data=initial, start=2, end=6, frequency=1)

ts(data=initial, start=2, end=5, frequency=1)

ts(data=initial, start=2, end=5, frequency=2)

# plot vector of values in 'initial' datafile....
# [,] means include all columns and rows of data
# type 'l' means 'lines'
plot(initial[,], type="l")

