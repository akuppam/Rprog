setwd("~/Pythprog/Rprog/data")

library(dplyr)
library(ggplot2)

# installed "timeSeries" package (install.packages("timeSeries"))
library(timeSeries)
library(ts)

tui <- read.csv("tui.csv", header = TRUE, dec = ",", sep = ";")

colnames(tui)
summary(tui)
View(tui)

# plot vector of values in 'initial' datafile....
# [,] means include all columns and rows of data
# [,5] means include all rows of data and 5th column of data
# type 'l' means 'lines'
# type 'p' means 'points'
# type 'b' means both - 'lines and points'
plot(tui[,5], type="l")
plot(tui[,2], type="b", col="blue", lwd=1, 
     xlab="Time", ylab="Open Values", 
     main="TUI Opening Values", ylim=c(0,60))

# diff() calculates the differences between all consecutive values of a vector
plot(diff(log(tui[,5])), type="l", col="red")

# plot histogram and lines over it
# prob = T is to use frequencies
hist(diff(tui[,4]), prob=T, type="l", col="red")
lines(density(diff(tui[,4])),lwd=2)

# In order to determine normality graphically, we can use the output of 
# a normal Q-Q Plot. If the data are normally distributed, the data points 
# will be close to the diagonal line. If the data points stray from the 
# line in an obvious non-linear fashion, the data are not normally distributed. 
qqnorm(diff(log(tui[,1])))

# If the p value of the Shapiro-Wilk Test is greater than 0.05, 
# the data is normal. If it is below 0.05, the data significantly deviate 
# from a normal distribution.
shapiro.test(diff(log(tui[,1])))


# plot 3 lines with 3 different moving averages (2, 12, 40)
# (1 / (2a + 1)) where a is 2, 12 or 40......

plot(tui[,5], type="l")
tui1 <- filter(tui[,5], rep(1/5,5))
tui2 <- filter(tui[,5], rep(1/25,25))
tui3 <- filter(tui[,5], rep(1/81,81))
lines(tui1, col="red")
lines(tui2, col="purple")
lines(tui3, col="blue")

# beer.csv has beer production from Jan 1956 to Aug 1995 in 1st column
# 'start' is labeling the first time point or record, while 'freq' is making it a year

beer <- read.csv("C:/Users/akuppam/Documents/Pythprog/Rprog/data/beer.csv", header=T, dec=",", sep=";")
View(beer)
beer <- ts(beer[,1], start=1956,freq=12)
View(beer)

# stl() - Seasonal Decomposition of Time Series by Loess
plot(stl(log(beer), s.window="periodic"))

colnames(beer)
summary(beer)

lbeer <- log(beer)
t <- seq(1956, 1995.2, length=length(beer))
t2 <- t^2
plot(lbeer)
lm(lbeer~t+t2)
plot(lm(lbeer~t+t2)$fit, type="l", col="red", lwd=1)

beer.lm <- lm(lbeer~t+t2)
# returns the coefficients of the linear log model of beer sales
# t and t-square are variables and their coefficients are estimated
# log(x) = alpha0 + alpha1 * t + alpha2 * t^2
lm(lbeer~t+t2)$coefficients
lines(lm(lbeer~t+t2)$fit, col="green", lwd=20)
plot(lm(lbeer~t+t2)$fit, type="l", col="red", lwd=1)

# using the lm function to predict 12 periods ahead
predict(beer.lm, n.ahead=48)
plot(beer,xlim=c(1956,2020))
plot(predict(beer.lm,n.ahead=192),col=2)

lines(predict(beer.lm,n.ahead=192),col=2)

# exponential smoothing
# fitting an exponential function to the time series data
plot(beer)
lines(HoltWinters(beer)$fitted,col="red")

# using the exponential function to predict 12 years ahead
beer.hw <- HoltWinters(beer)
predict(beer.hw, n.ahead=12)

# *********************************

library(rdatamarket)
library(Quandl)
ausgdp <- as.ts(dmseries("http://data.is/1jDQwpr")[,1])
ausgdp2 <- ts(rev(Quandl("FRED/AUSRGDPC", type="ts")), end=2011)
View(ausgdp2)
# **********************************

data("lynx")
View(lynx)
summary(lynx)
xyplot(lynx)
plot(lynx)
qplot(lynx)

# plot histogram and lines over it
# prob = T is to use frequencies
# diff() calculates the differences between all consecutive values of a vector
hist(diff(lynx), prob=T, type="l", col="red")
lines(density(diff(lynx)),lwd=2)
diff(lynx)
View(diff(lynx))

# In order to determine normality graphically, we can use the output of 
# a normal Q-Q Plot. If the data are normally distributed, the data points 
# will be close to the diagonal line. If the data points stray from the 
# line in an obvious non-linear fashion, the data are not normally distributed. 
qqnorm(diff(log(lynx)))

# If the p value of the Shapiro-Wilk Test is greater than 0.05, 
# the data is normal. If it is below 0.05, the data significantly deviate 
# from a normal distribution.
shapiro.test(diff(log(lynx)))

# plot 3 lines with 3 different moving averages (2, 12, 40)
# (1 / (2a + 1)) where a is 2, 12 or 40......

plot(lynx, type="l")
lynx1 <- filter(lynx, rep(1/5,5))
lynx2 <- filter(lynx, rep(1/25,25))
lynx3 <- filter(lynx, rep(1/81,81))
lines(lynx1, col="red")
lines(lynx2, col="purple")
lines(lynx3, col="blue")

# ***********************************
# MAG classification 15-minute counts
# ***********************************

setwd("~/MAG_LM_TruckTour/Counts")
classcnts <- read.csv("MAG_Classification_Counts.csv", header = TRUE, dec = ",", sep = ",")
View(classcnts)
plot(classcnts$X0.00)

classcnts1 <- classcnts[11:100]
View(classcnts1)
summary(classcnts1)

classcnts1 <- t(classcnts1)
View(classcnts1)
summary(classcnts1)

# plot vector of values in 'initial' datafile....
# [,] means include all columns and rows of data
# [,5] means include all rows of data and 5th column of data
# type 'l' means 'lines'
# type 'p' means 'points'
# type 'b' means both - 'lines and points'
plot(classcnts1[,1], type="l")
plot(classcnts1[,1], type="b", col="blue", lwd=1, 
     xlab="Time", ylab="PC Counts", 
     main="T1001, I-10, EB, PC Counts", ylim=c(0,2000))

plot(classcnts1[,2], type="b", col="blue", lwd=1, 
     xlab="Time", ylab="PC Counts", 
     main="T1001, I-10, EB, PC Counts", ylim=c(0,2000))

plot(classcnts1[,1], classcnts1[,2], type="l", col="blue", lwd=1, 
     xlab="Time", ylab="PC Counts", 
     main="T1001, I-10, EB, PC Counts", ylim=c(0,2000))

# develop a time series model (moving avg, or something else, like ARMA, ARIMA, etc
# predict future values, and plot the predictions to see the future trends

library(tabplot)
tableplot(classcnts[,11:20])

ggplot(data=classcnts, aes(x=SN, y=X0.00, group = Vehicle_Class, colour = as.factor(Vehicle_Class)))

# **********************
# use of ts()
# convert data to time series format for graphics
# *********************

# classcnts = has time series data across columns
# classcnts1 = has time series data on every rown (that is, in a single column vector)
# classcnts 2 = converts the columnular data into time series object/format using ts()

# frequency is key here - here it shows 4 periods per hour
# without 'frequency', the stl() will not show the seasonal distributions
classcnts2 <- ts(data = classcnts1[,1], start = 1, frequency = 4)
View(classcnts2)

# seasonal distribution function
plot(classcnts2)
plot(stl(log(classcnts2), s.window="periodic"))
plot(stl((classcnts2), s.window="periodic"))

# convert all data to time series objects but can plot only 10 at a time
# this code plots 10 plots side by side but in different plots
classcnts3 <- ts(data = classcnts1[,1:100], start = 1, frequency = 4)
View(classcnts3)
plot(classcnts3[,1:10])
plot(stl(log(classcnts3), s.window="periodic"))
plot(stl((classcnts3), s.window="periodic"))

# **************
# ARIMA 
# *************

fit <- arima(classcnts1[,1], order = c(1,0,1))
fit <- arima(classcnts1[,1], order = c(2,1,1))
fit <- arima(classcnts1[,1], order = c(2,0,1))    # (2,0,1) works well in short-term
fit <- arima(classcnts1[,1], order = c(3,0,1))
fit <- arima(classcnts1[,1], order = c(0,1,1))
fit <- arima(classcnts1[,1], order = c(2,0,2))    # (2,0,2) also works well in short-term
cnt.pred <- predict(fit, n.ahead=100)

fit
cnt.pred

plot(classcnts1[,1],xlim=c(0,200),ylim=c(0,1500))
cnt.pred<-predict(fit,n.ahead=100)
lines(cnt.pred$pred,col="red") 
lines(cnt.pred$pred+2*cnt.pred$se,col="red",lty=3) 
lines(cnt.pred$pred-2*cnt.pred$se,col="red",lty=3)

View(cnt.pred)

# --------------
plot(LakeHuron,xlim=c(1875,1980),ylim=c(575,584)) 
LH.pred<-predict(fit,n.ahead=8) 
lines(LH.pred$pred,col="red") 
lines(LH.pred$pred+2*LH.pred$se,col="red",lty=3) 
lines(LH.pred$pred-2*LH.pred$se,col="red",lty=3)
# --------------
# ARIMA(p,d,q)
# p - PACF chart - first time PACF curve crosses the upper confience level of 0.2 gives an indication of p=2
# p = no of auto regressive terms (AR)
# q - ACF chart - first time ACF curve crosses the upper confience level of 0.2 gives an indication of q=2
# q = no of moving average terms (MA)
# d = 0 or 1 may not matter that much (no of differences)


acf(diff(classcnts1[,1]))
pacf(diff(classcnts1[,1]))

# ********************************
# next - handout on EuStockMerkets
# ********************************
# decompose() and stl() functions
# ********************************
# refer to anything Robert Hyndman (github, google, etc) for time series in R
# ********************************

decomposedRes <- decompose(classcnts2, type = "mult")
plot(decomposedRes)

decomposedRes <- decompose(classcnts2, type = "additive")
plot(decomposedRes)

stlRes <- stl(classcnts2, s.window = "periodic")
stlRes
plot(stlRes)

library(forecast)
seasonplot(classcnts1[,1], 90, rainbow(90))
seasonplot(classcnts1[,2], 90, rainbow(90))
seasonplot(classcnts1[,3], 90, rainbow(90))
seasonplot(classcnts1[,4], 90, rainbow(90))
seasonplot(classcnts1[,5], 90, rainbow(90))

seasonplot(classcnts2, 4, rainbow(4))
seasonplot(classcnts2, ylab = "Count", year.labels = TRUE, year.labels.left = TRUE,
           labelgap = 0.8, col = 1:20, pch = 15, lwd = 3)

# -----------------
seasonplot(data, ylab = "Units Sold", year.labels = TRUE, year.labels.left = TRUE,
           labelgap = 0.8, col = 1:20, pch = 15, lwd = 3)
# -----------------

# *****************************
# create lead and lag variables to model future trends
# ****************************

library(DataCombine)

# ***************************
# Automated Forecasting
# The forecast package provides functions for the automatic selection of exponential and ARIMA models. 
# The ets() function supports both additive and multiplicative models. The auto.arima() function can 
# handle both seasonal and nonseasonal ARIMA models. Models are chosen to maximize one of several fit criteria.
# ***************************

library(forecast)
# Automated forecasting using an exponential model
fit.exp <- ets(classcnts2)
fit.exp

plot(classcnts2,xlim=c(0,100),ylim=c(0,1500))
cnt.predexp <- forecast(fit.exp, h=100)
plot(forecast(fit.exp, h = 100, level = 95), xlim=c(0,100),ylim=c(0,1500))    # 50, 80, 95 confidence intervals
plot(forecast(fit.exp, h = 100, fan = TRUE ))    # 50, 80, 95 confidence intervals

cnt.predexp
View(cnt.predexp)

# Automated forecasting using an ARIMA model
fit.arima <- auto.arima(classcnts2)
fit.arima

plot(classcnts2,xlim=c(0,50),ylim=c(0,1500))
cnt.predarima <- predict(fit.arima, n.ahead=50)
lines(cnt.predarima$pred,col="red") 
lines(cnt.predarima$pred+2*cnt.predarima$se,col="red",lty=3) 
lines(cnt.predarima$pred-2*cnt.predarima$se,col="red",lty=3)
cnt.predarima
View(cnt.predarima)

