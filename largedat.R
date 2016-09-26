# *************************************
# https://www.dezyre.com/apache-spark-tutorial/pyspark-tutorial
# *************************************

setwd("~/Pythprog/spark/ml-20m/ml-20m")
largedat <- read.csv("ratings.csv", sep = ",", header = TRUE) 
# took 1 min:38.54 sec
summary(largedat)
View(largedat)
