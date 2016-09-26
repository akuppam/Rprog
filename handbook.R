# A Handbook of Statistical Analyses Using R (HSAUR)
# Everitt & Hothorn
# installed HSAUR package

install.packages("HSAUR")
data("Forbes2000", package="HSAUR")
summary(Forbes2000)
head(Forbes2000)
str(Forbes2000)     # output vars, type of var (char, num, int, factor, etc), first few values
dim(Forbes2000)     # output no. of observations and no. of vars
nrow(Forbes2000)
ncol(Forbes2000)
names(Forbes2000)
table(Forbes2000[,"category"])    # shows a freq by each category
mean(Forbes2000[,"sales"])

#library("RODBC")      # open data base connectivity - to read excel and access files directly
#cnct <- odbcConnectExcel("Forbes2000.xls")

#list.files(pattern = ".rda")

companies <- Forbes2000[,"name"]
#print (companies)
print (companies[1:3])            # first three companies
print (companies[-(4:2000)])      # -ve sign indicates show everything but the ones in the brackets

order_sales <- order(Forbes2000$sales)     # sort ascending of sales
print (order_sales[1:5])
str(Forbes2000)     # output vars, type of var (char, num, int, factor, etc), first few values
companies[order_sales[1:5]]                # print out least 5 sales companies 

salescom <- Forbes2000[,"sales"]
print (salescom[1:5])
salescom[order_sales[1:5]]                 # print out least 5 sales values

print (order_sales[1995:2000])

# Using logical operator and printing only those rows that satisfies the logic
asset1k <- Forbes2000[Forbes2000$assets > 1000, c("name", "assets", "sales", "profits")]
print (asset1k)

# outputs summary; lapply and sapply do the same thing
lapply(Forbes2000, summary)
sapply(Forbes2000, summary)

# sort of like pivot table
# outputs 'median' "profits" of all categories ("category")
# sequence of "profits" and "category" does not matter in the function
mprofits <- tapply(Forbes2000$profits, Forbes2000$category, median, na.rm=TRUE)
print (mprofits)

mprofits <- tapply(Forbes2000$category, Forbes2000$profits, median, na.rm=TRUE)
print (mprofits)

str(Forbes2000)     # output vars, type of var (char, num, int, factor, etc), first few values

hist(Forbes2000$marketvalue)
hist(log(Forbes2000$marketvalue))

# pch is plotting character (points, colors, etc)
plot(log(Forbes2000$marketvalue) ~ log(Forbes2000$sales), pch=".")

# boxplot for certain countries
boxplot(log(marketvalue) ~ country, data = subset(Forbes2000, country %in% c("united Kingdom", "India", "Germany", "Turkey")), ylab = "log(marketvalue)", varwidth = TRUE)

qqnorm(Forbes2000$marketvalue)
qqline(Forbes2000$marketvalue)

#t.test(marketvalue ~ sales, data=Forbes2000, var.equal=TRUE)

###################################################
###################  2nd Chapter ##################
###################################################

data1 <- data("roomwidth", package="HSAUR")
summary(data1)
head(data1)

summary("roomwidth")
head("roomwidth")
print ("roomwidth")
str("roomwidth")

tapply(roomwidth$width, roomwidth$unit, summary)
names("roomwidth")
dim("roomwidth")
nrow("roomwidth")

t.test(I(width) ~ unit, data=roomwidth, var.equal=TRUE)

data("waves", package="HSAUR")
summary("waves")

#######################

data("weightgain", package="HSAUR")
summary(weightgain)
head(weightgain)

lapply(weightgain, summary)
sapply(weightgain, summary)
table(weightgain)
str(weightgain)
str(roomwidth)

plot.design(weightgain)  # shows a plot with a 'mean' line for all the factor variables
plot.design(roomwidth)
mean(weightgain$weightgain, source=Cereal)
mean(weightgain$weightgain, source=Beef)
mean(weightgain$weightgain, source=High)
mean(weightgain$weightgain, source=Low)

#################################################
####### chapter 4 ##############
################################################

setwd("~/MSDOT_LRTP/Data/ATRI")

library(dplyr)
library(ggplot2)
library(lattice)

initial <- read.csv("MSDOT_ExtractedTruckTrips_Draft 2.csv", header = TRUE)

initialNew3 <- data.frame(initial$TripLength, initial$TripTime, initial$TripSpeed, initial$PointsStop)
head(initialNew3)

pairs(initialNew3)

#pairs(initialNew3, colors(distinct = TRUE))

#pairs(initialNew3, col == c("red","blue","yellow", "green"))

# Plot #2: same as above, but add loess smoother in lower and correlation in upper
#pairs(~initial$TripLength+initial$TripTime+initial$TripSpeed+initial$PointsStop,
#      lower.panel=panel.smooth, upper.panel=panel.cor, 
#      pch=20, main="MS ATRI Scatterplot Matrix")

#plotmatrix(initialNew3, colorRampPalette(colors = "red", "blue", "black", "green"))
#pairs(initialNew3) + geom_smooth(method="lm")

#scatterplot.matrix(~initial$TripLength+initial$TripTime+initial$TripSpeed+initial$PointsStop,main="MS ATRI")

# Spinning 3d Scatterplot
library(rgl)
plot3d(initialNew3$TripLength, initialNew3$TripSpeed, initialNew3$PointsStop, col="red", size=3)
plot3d(initialNew3$TripLength, initialNew3$TripSpeed, initialNew3$TripTime, col="red", size=3)

plot3d(Forbes2000$profits, Forbes2000$marketvalue, Forbes2000$sales, col="red", size=3)

# Another Spinning 3d Scatterplot
library(Rcmdr)
#attach(mtcars)
plot3d(initialNew3$TripLength, initialNew3$TripSpeed, initialNew3$PointsStop, col="red", size=3)
plot3d(initialNew3$TripLength, initialNew3$TripSpeed, initialNew3$TripTime, col="red", size=3)

scatter3d(initialNew3$TripLength, initialNew3$TripSpeed, initialNew3$PointsStop)
scatter3d(Forbes2000$profits, Forbes2000$marketvalue, Forbes2000$sales)

##################################################
#########  chapter 5   #########################
################################################

install.packages("HSAUR")
data("clouds", package = "HSAUR")
summary(clouds)   # always use the name of the data file WITH NO QUOTES
head(clouds)
str(clouds)
dim(clouds)     # output no. of observations and no. of vars
nrow(clouds)
ncol(clouds)
names(clouds)
#table(Forbes2000[,"category"])    # shows a freq by each category
#mean(Forbes2000[,"sales"])

library(rgl)
plot3d(clouds$rainfall, clouds$seeding, clouds$cloudcover)

pairs(clouds)

psymb <- as.numeric(clouds$seeding)    # giving different symbols to categorical var like seeding (no=1, yes=2)
plot(rainfall ~ cloudcover, data = clouds, pch = psymb)
abline(lm(rainfall ~ cloudcover, data = clouds, subset = seeding == "no"))  # abline is adding line to a plot
abline(lm(rainfall ~ cloudcover, data = clouds, subset = seeding == "yes"), lty=2)  # abline is adding line to a plot
legend("topright", legend = c("no seeding", "seeding"), pch=1:2, lty=1:2, bty="n")





