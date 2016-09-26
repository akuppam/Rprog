setwd("~/Pythprog/Rprog")

library(dplyr)
library(ggplot2)
library(DAAG)
library(e1071)

exdata <- read.csv("linreg.csv", header = TRUE)

colnames(exdata)
rownames(exdata)
summary(exdata)

# scatter plot of x and y variable; "main" gives a title to the plot
scatter.smooth(x=exdata$v2, y=exdata$v3, main="v2 ~ v3")

# par divides the plot into two columns
par(mfrow=c(1,2))
boxplot(exdata$v2, main="v2", sub=paste("Outliers:", boxplot.stats(exdata$v2)$out))
boxplot(exdata$v3, main="v3", sub=paste("Outliers:", boxplot.stats(exdata$v3)$out))

# load R's statistics package
library(e1071)

# divide graph area in 2 columns
par(mfrow=c(1, 2))   

# density plot for 'Distance'
plot(density(exdata$v2), main="Density Plot: v2", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(exdata$v2), 2)))
polygon(density(exdata$v2), col="blue") 

# density plot for 'AirTime' 
plot(density(exdata$v3), main="Density Plot: v3", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(exdata$v3), 2)))
polygon(density(exdata$v3), col="red")

# compute correlation
cor(exdata$v2, exdata$v3)

# developing a linear regression model
linearmod <- lm(v2 ~ v3, data=exdata)
print(linearmod)
summary(linearmod)

# create training and test data
# training data - to use 80% of data and use it to develop a model
# test data - use 20% of the data, apply the developed model on it to see predicted values
set.seed(100)
trainingRowIndex <- sample(1:nrow(exdata), 0.8*nrow(exdata))
trainingData <- exdata[trainingRowIndex, ]
testData <- exdata[-trainingRowIndex, ]

predMODEL <- lm(v2 ~ v3, data=trainingData)
predDIST <- predict(predMODEL, testData)

summary(predMODEL)
AIC(predMODEL)
BIC(predMODEL)

# predicting using the estimated model
actuals_preds <- data.frame(cbind(actuals=testData$v2, predicteds = predDIST))
correlation_accuracy <- cor(actuals_preds)
head(actuals_preds)

mape <- mean(abs(actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals
print ("Mean Absolute Percentage Deviation:", mape)
print (mape)
print.default("Mean Absolute Percentage Deviation:", mape)

library(DAAG)
#cvResults <- suppressWarnings(CVlm(df=flights, form.lm=Distance ~ AirTime, m=5, dots=FALSE, seed=29, legend.pos="topleft", printit=FALSE, main="Small symbols are predcited values while bigger ones are actuals."))
#cvResults <- CVlm(data=flights, form.lm=Distance ~ AirTime, m=5, dots=FALSE, seed=29, legend.pos="topleft", printit=FALSE, main="Small symbols are predcited values while bigger ones are actuals.")
cvResults <- CVlm(data=trainingData, form.lm=v2 ~ v3)
attr(cvResults, 'ms')

CVlm(data=exdata, form.lm=v2 ~ v3,
     plotit="Observed")
CVlm(data=exdata, form.lm=v2 ~ v3,
     plotit="Residual")
out <- CVlm(data=exdata, form.lm=v2 ~ v3,
            plotit="Observed")
out[c("ms","df")]

library(DAAG)
CVlm(data = DAAG::exdata, form.lm = v2 ~ v3,
     m = 3, dots = FALSE, seed = 29, plotit = c("Observed","Residual"),
     main="Small symbols show cross-validation predicted values",
     legend.pos="topleft", printit = TRUE)
cv.lm(data = DAAG::exdata, form.lm = v2 ~ v3,
      m = 3, dots = FALSE, seed = 29, plotit = c("Observed","Residual"),
      main="Small symbols show cross-validation predicted values",
      legend.pos="topleft", printit = TRUE)
