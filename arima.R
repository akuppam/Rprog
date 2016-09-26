setwd("~/Pythprog/Rprog/data")

library(dplyr)
library(ggplot2)

# installed "timeSeries" package (install.packages("timeSeries"))
library(timeSeries)
library(ts)


# not sure yet what c(0.4, 0.4) can do - research more !
# plots auto corrlations and partial correlations
# par(mfrow=c(2,2)) splits the screen into 4 (or 2 x 2) and plots 4 charts
sim.ar <- arima.sim(list(ar=c(0.4, 0.4)), n=10)
print(sim.ar)
par(mfrow=c(2,2))
acf(sim.ar, main="ACF of AR2 process")
pacf(sim.ar, main="PACF of AR2 process")

tui <- read.csv("tui.csv", header = TRUE, dec = ",", sep = ";")

colnames(tui)
summary(tui)
par(mfrow=c(2,1))
plot(tui[,5], type="l")

# fits an arima model of the order 1,0,0
# predicts next 8 years
# plots forecasts (in red  color) +/- 2# std. error (se)
fit <- arima(tui[,5], order = c(1,0,0))
forecasts <- predict(fit, n.ahead=8)
lines(forecasts$pred, col="red")
lines(forecasts$pred+2*forecasts$se, col="red")
lines(forecasts$pred-2*forecasts$se, col="red")
