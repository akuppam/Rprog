library(dplyr)
library(ggplot2)
library(DAAG)
library(e1071)

library(hflights)

flights <- tbl_df(hflights)
colnames(flights)
#rownames(flights)
summary(flights)

# scatter plot of x and y variable; "main" gives a title to the plot
scatter.smooth(x=flights$Distance, y=flights$AirTime, main="Distance ~ AirTime")

scatter.smooth(x=flights$DepTime, y=flights$ArrTime, main="Dep ~ Arr")

# par divides the plot into two columns
par(mfrow=c(1,2))
boxplot(flights$Distance, main="DISTANCE", sub=paste("Outliers:     ", boxplot.stats(flights$Distance)$out))
boxplot(flights$AirTime, main="AIR TIME", sub=paste("Outliers:    ", boxplot.stats(flights$AirTime)$out))

# load R's statistics package
library(e1071)

# divide graph area in 2 columns
par(mfrow=c(1, 2))   

# density plot for 'Distance'
plot(density(flights$Distance), main="Density Plot: DISTANCE", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(flights$Distance), 2)))
polygon(density(flights$Distance), col="blue") 

# density plot for 'AirTime' 
plot(density(flights$AirTime, na.rm=TRUE), main="Density Plot: AIR TIME", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(flights$AirTime), 2)))
polygon(density(flights$AirTime, na.rm=TRUE), col="red") 

# compute correlation
cor(flights$Distance, flights$DayOfWeek)

# developing a linear regression model
linearmod <- lm(Distance ~ AirTime, data=flights)
print(linearmod)
summary(linearmod)

# the following is not necessary - useful only if you want to compute individual stats like p=value, etc
modelSummary <- summary(linearmod)
modelCoeffs <- modelSummary$Coefficients
beta.estimate <- modelCoeffs ["AirTime", "Estimate"]
std.error <- modelCoeffs ["AirTime", "Std. Error"]
t_value <- beta.estimate/std.error

# create training and test data
# training data - to use 80% of data and use it to develop a model
# test data - use 20% of the data, apply the developed model on it to see predicted values
set.seed(100)
trainingRowIndex <- sample(1:nrow(flights), 0.8*nrow(flights))
trainingData <- flights[trainingRowIndex, ]
testData <- flights[-trainingRowIndex, ]

predMODEL <- lm(Distance ~ AirTime, data=trainingData)
predDIST <- predict(predMODEL, testData)

summary(predMODEL)
AIC(predMODEL)
BIC(predMODEL)

# predicting using the estimated model
actuals_preds <- data.frame(cbind(actuals=testData$Distance, predicteds = predDIST))
correlation_accuracy <- cor(actuals_preds)
head(actuals_preds)

mape <- mean(abs(actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals
print ("Mean Absolute Percentage Deviation:", mape)
print (mape)
print.default("Mean Absolute Percentage Deviation:", mape)

library(DAAG)
#cvResults <- suppressWarnings(CVlm(df=flights, form.lm=Distance ~ AirTime, m=5, dots=FALSE, seed=29, legend.pos="topleft", printit=FALSE, main="Small symbols are predcited values while bigger ones are actuals."))
#cvResults <- CVlm(data=flights, form.lm=Distance ~ AirTime, m=5, dots=FALSE, seed=29, legend.pos="topleft", printit=FALSE, main="Small symbols are predcited values while bigger ones are actuals.")
cvResults <- CVlm(data=trainingData, form.lm=Distance ~ AirTime)
attr(cvResults, 'ms')

CVlm(data=flights, form.lm=Distance~AirTime,
     plotit="Observed")
CVlm(data=flights, form.lm=Distance~AirTime,
     plotit="Residual")
out <- CVlm(data=flights, form.lm=Distance~AirTime,
            plotit="Observed")
out[c("ms","df")]

library(DAAG)
CVlm(data = DAAG::flights, form.lm = Distance ~ AirTime,
     m = 3, dots = FALSE, seed = 29, plotit = c("Observed","Residual"),
     main="Small symbols show cross-validation predicted values",
     legend.pos="topleft", printit = TRUE)
cv.lm(data = DAAG::flights, form.lm = Distance ~ AirTime,
      m = 3, dots = FALSE, seed = 29, plotit = c("Observed","Residual"),
      main="Small symbols show cross-validation predicted values",
      legend.pos="topleft", printit = TRUE)
