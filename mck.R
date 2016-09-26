Sys.time()
mckstarttime = Sys.time()

setwd("~/Pythprog/Rprog")
mck <- read.csv("mckdata.csv", header = TRUE)
summary(mck)
str(mck)

library(dplyr)
library(ggplot2)
library(devtools)
library(yarrr)
library(beanplot)

par(mfrow = c(2, 2))

scatter.smooth(mck$lat,mck$long)
plot(mck$time, mck$dist)
boxplot(ht~wt, data = mck)
boxplot(ht~abc, data = mck)

beanplot(tours~sector, data = mck)

pirateplot(formula = tons ~ naics,
           data = mck,
           pal = "google",
           bean.o = .4,
           back.col = transparent("blue",.9),
           line.fun = mean,
           line.lwd = 8,
           gl.col = gray(.5),
           bar.o = (.5),
           bar.border.col = gray(.9),
           xlab = "naics",
           ylab = "tons",
           main = "Tons by NAICS")

pirateplot(formula = tons ~ atype,
           data = mck,
           pal = "google",
           bean.o = .4,
           back.col = transparent("blue",.9),
           line.fun = mean,
           line.lwd = 8,
           gl.col = gray(.5),
           bar.o = (.5),
           bar.border.col = gray(.9),
           xlab = "atype",
           ylab = "tons",
           main = "Tons by Area Type")

pirateplot(formula = tons ~ abc,
           data = mck,
           pal = "google",
           bean.o = .4,
           back.col = transparent("blue",.9),
           line.fun = mean,
           line.lwd = 8,
           gl.col = gray(.5),
           bar.o = (.5),
           bar.border.col = gray(.9),
           xlab = "abc",
           ylab = "tons",
           main = "Tons by ABC")

print(paste("Total Run Time:", (Sys.time()-mckstarttime)))
Sys.time()
#print(paste("Total Run Time:",round((Sys.time()-mckstarttime)/60),"minutes.",sep=" "))


#jpg(file="~/Pythprog/Rprog/mck1.jpeg") 
#mytitle = paste("Tons by ABC")
#plot(x,y, main = mytitle) 
#dev.off() 

# *********************************
# factor to numeric
# *********************
mck$abc1 <- as.numeric(factor(mck$abc,levels = c("A","B","C","D","E","F","G",
                                                 "H","I","J","K","L","M","N",
                                                 "O","P","Q","R","S","T","U",
                                                 "V","W","X","Y","Z")))

summary(mck$abc1)
summary(mck)

# *********************************
# aggregating up to finite groups
# *********************************
mck$abccat <- cut(mck$abc1,c(0,5,10,15,20,26))
summary(mck$abccat)
summary(mck)

# *********************************
# frequency & cross-tabulations
# *********************

mcktables <- table(mck$atype, mck$abccat)
mcktables
sum(mcktables)

margin.table(mcktables, 1) # sum totals of atype categories
margin.table(mcktables, 2) # sum totals of abccat categories

prop.table(mcktables) # percent of grant total
prop.table(mcktables, 1) # percent of row totals
prop.table(mcktables, 2) # percent of col totals

#ftable(magtables3way)
# LOOK INTO WRITE FREQ TABLES TO CSV

###########################

# Plotting v/c by atype and sector
ggplot(mck, aes(x = sector, y = vc)) + 
  geom_line(aes(group = atype, color = atype)) + xlab('Sector') + ylab('V/C') + 
  ggtitle('Atype Sector V/C')

# heatmap
ggplot(mck, aes(x = sector, y = vc, fill = value, color = NULL))+ 
  geom_bin2d()+xlab("Sector") + ylab("V/C") +  
  ggtitle("Atype Sector V/C 1")
#  scale_fill_gradient(low="green", high="red")
#  scale_y_discrete(limits = rev(levels(mck$atype))) 

# *****************
# LASSO REGRESSION
# ****************

library(glmnet)

xfactors <- model.matrix(mck$abc1 ~ mck$tours + mck$vmt + mck$sector)
x <- as.matrix(data.frame(xfactors)) # stores all the (N=1000) values from the above 3 vars
summary(x)

glmmod<-glmnet(x,y=as.factor(mck$abc1),alpha=1,family='multinomial')
plot(glmmod,xvar="lambda")
glmmod
coef(glmmod,10)

# ***********
# cross validation
# **************
cv.glmmod <- cv.glmnet(x,y=mck$abc1,alpha=1)
cv.glmmod
plot(cv.glmmod)
best_lambda <- cv.glmmod$lambda.min
best_lambda

# *****************
# LINEAR REGRESSION
# ****************

# model

olsmodel <- lm(vmt ~ trips + dist, data = mck)
summary(olsmodel)
attributes(olsmodel)

par(mfrow=c(1,1))
plot(mck$trips + mck$dist, mck$vmt)

summary(mck$trips)
summary(mck$dist)
summary(mck$vmt)

# predict

preddata <- data.frame(trips=c(3,10,24), dist=c(30,100,240))
preddata
predict(olsmodel,preddata,interval='confidence')
predres <- predict(olsmodel,mck,interval='confidence')
write.csv(predres,"predres.csv")

summary(predres)
str(predres)
head(predres)

predmck <- data.frame(mck, predres) # append addl columns to the end of the data
head(predmck,100)
summary(predmck)

# model vs observed - plots

scatter.smooth(predmck$vmt,predmck$fit)

# Predicted vs. actual for each model
ggplot(data = predmck,aes(x = predmck$vmt, y = predmck$fit)) + 
  geom_point(colour = "blue") + 
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  geom_vline(xintercept = 23, colour = "green", linetype = "dashed") +
#  coord_cartesian(xlim = c(0,70),ylim = c(0,70)) +
  ggtitle("Predicted vs. Actual, by model")


qplot(x=vmt, y=fit, data = predmck)+
  geom_bin2d()+xlab("Model")+ylab("Observed") +  
  ggtitle("Model vs Observed") +
  geom_point(colour = "red") + 
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  geom_vline(xintercept = 23, colour = "green", linetype = "dashed")
  #  scale_fill_gradient(low="green", high="red")
#  scale_y_discrete(limits = rev(levels(predmck$atype)))

# *******************************************
# training (70%) and testing (30%) datasets
# http://www.r-bloggers.com/part-4a-modelling-predicting-the-amount-of-rain/
# *******************************************

set.seed(100) # random seed 100; can be anything but use the same for reproducability
seventy <- sample(1:nrow(mck),0.70*nrow(mck))
train <- mck[seventy,]
test <- mck[-seventy,]

summary(train)
summary(test)
nrow(train)
nrow(test)

# Create a dataframe with train and test indicator...
group <- rep(NA,1000)
group <- ifelse(seq(1,1000) %in% seventy,"Train","Test")
df <- data.frame(trips=mck$trips, dist=mck$dist, group)
summary(df)

# ...and plot it
ggplot(df,aes(x = trips,y = dist, colour = group)) + geom_point() +
  scale_color_discrete(name="") + theme(legend.position="top")

# *******************************************
# baseline model
# *******************************************

best.guess <- mean(train$vmt)
best.guess

# Evaluate RMSE and MAE on the testing data
RMSE.baseline <- sqrt(mean((best.guess-test$vmt)^2))
RMSE.baseline

# mean absolute error
MAE.baseline <- mean(abs(best.guess-test$vmt))
MAE.baseline

# *******************************************
# Linear Regression Model with training data
# *******************************************

olstrain <- lm(vmt ~ trips + dist, data = train)
summary(olstrain)

# predict with test data

testpredols <- predict(olstrain,test)
RMSE.ols <- sqrt(mean((testpredols-test$vmt)^2))
RMSE.ols
MAE.ols <- mean(abs(testpredols-test$vmt))
MAE.ols

# **************************************
# DECISION TREE
# does a cross-validation
# runs the model 10 times with 90% of data and tests it on 10% of data
# *************************************
library(rpart)
library(rattle)

decisiontrain <- rpart(vmt ~ trips + dist, data = train)
summary(decisiontrain)

testpreddecision <- predict(decisiontrain,test)
RMSE.decision <- sqrt(mean((testpreddecision-test$vmt)^2))
RMSE.decision
MAE.decision <- mean(abs(testpreddecision-test$vmt))
MAE.decision

printcp(decisiontrain)

min.xerror <- decisiontrain$cptable[which.min(decisiontrain$cptable[,"xerror"]),"CP"]
min.xerror

# ...and use it to prune the tree
decisiontrain.pruned <- prune(decisiontrain,cp = min.xerror)

# Plot the pruned tree
fancyRpartPlot(decisiontrain.pruned)

testpreddecisionpruned <- predict(decisiontrain.pruned,test)
RMSE.decisionpruned <- sqrt(mean((testpreddecisionpruned-test$vmt)^2))
RMSE.decisionpruned
MAE.decisionpruned <- mean(abs(testpreddecisionpruned-test$vmt))
MAE.decisionpruned

# ******************************
# RANDOM FOREST
# *****************************

library(randomForest)
set.seed(55)

randfortrain <- randomForest(vmt ~ trips + dist, data = train, importance=TRUE, ntree = 1000)
summary(randfortrain)

testpredrandfor <- predict(randfortrain,test)
RMSE.randfor <- sqrt(mean((testpredrandfor-test$vmt)^2))
RMSE.randfor
MAE.randfor <- mean(abs(testpredrandfor-test$vmt))
MAE.randfor

# **************************************
# comparing all the above models
# *************************************

all.predictions <- data.frame(actual = test$vmt,
                              baseline = best.guess,
                              linear.regression = testpredols,
                              full.tree = testpreddecision,
                              pruned.tree = testpreddecisionpruned,
                              random.forest = testpredrandfor)

summary(all.predictions)
head(all.predictions)

library(tidyr)

# Gather the prediction variables (columns) into a single row (i.e., wide to long)
# Recall the ggplot2 prefers the long data format

all.predictions <- gather(all.predictions,key = model,value = predictions,2:6)

summary(all.predictions)
head(all.predictions)
tail(all.predictions)
nrow(all.predictions)

# Predicted vs. actual for each model
ggplot(data = all.predictions,aes(x=all.predictions$actual, y=all.predictions$predictions)) + 
  geom_point(colour = "blue") + 
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  geom_vline(xintercept = 23, colour = "green", linetype = "dashed") +
  facet_wrap(~ model,ncol = 2) + 
  coord_cartesian(xlim = c(0,70),ylim = c(0,70)) +
  ggtitle("Predicted vs. Actual, by model")

qplot(x=actual, y=predictions, data = all.predictions)+
  geom_bin2d()+xlab("Actual")+ylab("Predictions") +  
  ggtitle("Predicted vs Actual, by Model") +
  geom_point(colour = "red") + 
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  geom_vline(xintercept = 23, colour = "green", linetype = "dashed")

# **************************************
# use of mlogit
# *************************************
library(Formula)
library(maxLik)
library(miscTools)
library(mlogit)

head(mck,10)

mcklogit <- mlogit.data(mck, shape = "wide", varying = NULL, choice = "abc")
head(mcklogit,5)
head(mck,5)
summary(mcklogit)
View(mcklogit)

write.csv(mcklogit, "mcklogit.csv")

# -1 (or 0) indicates no intercept
mnlmck <- mlogit(abc ~ tons + trips + atype | -1, mcklogit) 
summary(mnlmck)

mnlmck <- mlogit(abc ~ trips + atype | -1, mcklogit) 
summary(mnlmck)

mnlmck <- mlogit(abc ~ atype | -1, mcklogit) 
summary(mnlmck)

mnlmck <- mlogit(abc ~ ht + wt + vol | -1, mcklogit) 
summary(mnlmck)

mnlmck <- mlogit(abc ~ time + speed | -1, mcklogit) 
summary(mnlmck)

mnlmck <- mlogit(abc ~ time + dist | -1, mcklogit) 
summary(mnlmck)

# **************************
mcklogit1 <- mlogit.data(mck, shape = "wide", varying = NULL, choice = "atype")
head(mcklogit1,5)
head(mck,5)
summary(mcklogit1)

mnlmck1 <- mlogit(atype ~ tons + trips + vmt | -1, mcklogit1)
summary(mnlmck1)

mnlmck1 <- mlogit(atype ~ vmt | -1, mcklogit1)
summary(mnlmck1)

mnlmck1 <- mlogit(atype ~ vmt | abc | naics | sector, mcklogit1)
summary(mnlmck1)

# *********************************
# print out fitted/predicted values
# *********************************

head(fitted(mnlmck1))
head(fitted(mnlmck1, outcome = FALSE))

head(fitted(mnlmck))
head(fitted(mnlmck, outcome = FALSE))

# *******************************
# monte carlo simulation
# ******************************
summary(mck)
colnames(mck)
str(mck)

cor.test(mck$tons, mck$atype)
cor.test(mck$tours, mck$vmt)
cor.test(mck$atype, mck$vmt)

glmmodel <- glm(tours ~ trips + atype, data = mck)
summary(glmmodel)
lmmodel <- lm(tours ~ trips + atype, data = mck)
summary(lmmodel)

#Coefficients:
#               Estimate     Std. Error   t value Pr(>|t|)    
#(Intercept)   -8.584e-04  2.729e-04    -3.146   0.0017 ** 
#  trips        3.774e-01  1.309e-05 28832.358   <2e-16 ***
#  atype        8.033e-05  6.401e-05     1.255   0.2098    

set.seed(20)

#Predictor (q). 
#Use seq for generating equally spaced sequences fast 
q <- seq(from=0, to=20, by=0.1) 
q

#Value to predict (y): 
y <- (-8.584e-04) + (3.774e-01)*mck$trips + 8.033e-05 * mck$atype + (q-10)^3
summary (y)
summary(mck$tours)
head(y,30)


#Some noise is generated and added to the real signal (y): 
noise <- rnorm(length(q), mean=10, sd=80) 
noisy.y <- y + noise 

summary(y)
summary(noisy.y)
summary(q)

#Plot of the noisy signal: 
plot(y,noisy.y)
plot(y,noisy.y,col='deepskyblue4',xlab='q',main='Observed data') 
lines(y,noisy.y,col='firebrick1',lwd=3)

# ***************************************
ls()
