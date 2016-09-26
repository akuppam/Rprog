setwd("~/MSDOT_LRTP/Data/ATRI")

library(dplyr)
library(ggplot2)
library(mcsm)

initial <- read.csv("MSDOT_ExtractedTruckTrips_Draft 2.csv", header = TRUE)

#row.names(initial) 							# writing row names to the console (also counting no of rows)
colnames(initial) 								# writing variable or field names to the console

summary(initial)

# ******************************************************************
# separate OriginDtaeStamp (4/9/2014 16:48) into two new columns (4/9/2014) and (16:48)
# use strsplit() to split the column based on (" ") spaces in between the date and time
initial$OriginDateStamp <- as.character(initial$OriginDateStamp)
initialNew <- strsplit(initial$OriginDateStamp, " ")
colnames(initial)
colnames(initialNew)
head(initialNew)

# use do.call and rbind to append the two new variables into the main dataset 
initialNew2 <- data.frame(initial, do.call(rbind,initialNew))
colnames(initialNew2)
head(initialNew2)

# correlation of two variables
cor.test(initialNew2$TripLength, initialNew2$TripSpeed)

# glm() and lm() produces the same model and coeffs; lm() generates r-squared
model <- glm(TripSpeed ~ TripLength + TripTime, data = initialNew2)
summary(model)

model1 <- lm(TripSpeed ~ TripLength + TripTime, data = initialNew2)
summary(model1)

glimpse(initialNew2) 

data <- initialNew2

par(mfrow=c(2,2))  # 2 rows of charts, and 2 charts in each row
hSpeed <- hist(initialNew2$TripSpeed) # histogram
hDist <- hist(initialNew2$TripLength) # histogram
hTime <- hist(initialNew2$TripTime) # histogram
str(hSpeed) # returns breaks, counts, density, mids, xname, etc
str(hDist) # returns breaks, counts, density, mids, xname, etc
str(hTime) # returns breaks, counts, density, mids, xname, etc

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 44.403164   0.035453  1252.4   <2e-16 ***
#  TripLength   0.679065   0.001706   398.1   <2e-16 ***
#  TripTime    -0.572016   0.001765  -324.1   <2e-16 ***

set.seed(20)

#Predictor (q). 
#Use seq for generating equally spaced sequences fast 
q <- seq(from=0, to=20, by=0.1) 
q
View(q)

#Value to predict (y): 
y <- 44.403164 + 0.679065 * initialNew2$TripLength - 0.572016 * initialNew2$TripTime + (q-10)^3
summary (y)
head(y,30)
View(y)

#Some noise is generated and added to the real signal (y): 
noise <- rnorm(length(q), mean=10, sd=80)
head(noise,10)
noisy.y <- y + noise 
head(noisy.y,10)

summary(y)
summary(noisy.y)
summary(q)

#Plot of the noisy signal: 
plot(y,noisy.y)
plot(y,noisy.y,col='deepskyblue4',xlab='y',main='Observed data') 
lines(y,noisy.y,col='firebrick1',lwd=3)

###########################
# https://www.youtube.com/watch?v=-eYyN0YoFng&index=1&list=PLM4jCr0fwH5axpmWgoI31UGtDTQzeHuzq&spfreload=5
# Part 1 - R programming for simulation and monte carlo
###########################

runif(100,min=2,max=5)
runif(100)  # if min and max are NOT specified, the values returned are between 0 and 1
x = runif(100, min=2, max=5)
print (x)
x1 = x[-100];x1 # store everything in 'x' except the 100th value; adding a semi colon and var name, will print it
print (x1)
x2 = x[-1] # store everything in 'x' except the 1st value
print (x2)
plot(x1,x2)

x = runif(100)
x1 = x[-100]
x2 = x[-1]
x3 = x[-50]

par(mfrow=c(2,4))  # 2 rows of charts, and 4 charts in each row
plot(x1,x2)
plot(x1,x3)
plot(x2,x3)
hist(x)
hist(x1)
hist(x2)
hist(x3)

##############################

# same seed produces same set of random values all the time
set.seed(1)
runif(100,min=2,max=5)
set.seed(2)
runif(100,min=2,max=5)

################################
###########################
# https://www.youtube.com/watch?v=Lv-ofCiL5hI&index=2&list=PLM4jCr0fwH5axpmWgoI31UGtDTQzeHuzq
# Part 2 - R programming for simulation and monte carlo
###########################

temps = c(35, 48, 19, 62); temps
ozone = c(297, 125, 456, 321); ozone

oo = order(ozone); oo # returns position of the index after sorting the values
oo1 = sort(ozone); oo1

TEMPS1 = temps[oo]; TEMPS1 # returns 'temps' values in the order of indexes in 'oo'

x = matrix(1:16, 4, 4); x
x = matrix(1:16, 4, 4, byrow = TRUE); x
rownames(x) = letters(1:4)
colnames(x) = c("a", "b", "c", "d")
x

str (x)
nrow(x)
ncol(x)

x * x # multiplies each element
x ^ 2 # squares each element
x %*% x # does a matrix product or multiplication (sum product of 1st row & 1st column, etc)

################################
###########################
# https://www.youtube.com/watch?v=Lv-ofCiL5hI&index=2&list=PLM4jCr0fwH5axpmWgoI31UGtDTQzeHuzq
# Part 3 & 4 - R programming for simulation and monte carlo
###########################

k = c(initialNew2$TripSpeed) # Y or dependent variable
x = c(initialNew2$TripLength) # X or independent variable

barplot(x, names.arg = k, main = "Speed vs Length")

p = x/sum(x) # relative frequencies
print(p)
r = sum(k * p) # mean
v = sum(x * (k-r)^2) / (221828-1) # variance {str(initialNew2$TripSpeed) - for determining N value } 
print(r)
print(v)
f = dpois(k,r) # fitting a poisson distribution
print(cbind(k,p,f)) # this outputs k (dependent var), observed probability, and predicted probability

######################################
# ******  TEST OUT POISSON ***********
#####################################
# k = no of kicks
# x = count
# mean = r
# var = v

k = c(10, 20, 30, 40, 50) # Y or dependent variable
x = c(1.2, 2.5, 2.8, 4.3, 5.6) # X or independent variable

barplot(x, names.arg = k, main = "Y vs X") # plot the frequencies
h <- hist(x) # histogram for x
str(h) # returns breaks, counts, density, mids, xname, etc


p = x/sum(x) # relative frequencies
print(p)

r = sum(k * p) # mean
v = sum(x * (k-r)^2) / (221828-1) # variance {str(initialNew2$TripSpeed) - for determining N value } 
print(r)
print(v)

f = dpois(k,r) # fitting a poisson distribution
print(cbind(k,p,f)) # this outputs k (dependent var), observed probability, and predicted probability

mylist <- list(k=k, count=x, mean=r, var=v); mylist
str(mylist)

######################################
# Introducing Monte Carlo Methods with R.PDF
# Robert, Casella
#####################################

x=array(1:50,c(2,5,5)); x # 2 rows by 5 columns matrices, and 5 such matrices with values 1 to 50

d1 = runif(10); d1
d2 = rnorm(10); d2
out = t.test(d2)
out
names(out)

cor.test(d1,d2)

anova(lm(TripSpeed ~ TripTime, data = initialNew2))

s=matrix(0,ncol=9,nrow=9)
s
s[1,c(6,8)]=c(6,4) # row 1; col 6 and 8 (with values 6 and 4)
s[2,c(1:3,8)]=c(2,7,9,5)
s[3,c(2,4,9)]=c(5,8,2)
s[4,3:4]=c(2,6)
s[6,c(3,5,7:9)]=c(1,9,6,7,3)
s[7,c(1,3:4,7)]=c(8,5,2,4)
s[8,c(1,8:9)]=c(3,8,5)
s[9,c(1,7,9)]=c(6,9,1)
s

# --------------------
x = seq(90:110)
length(x)
x

y = x +rnorm(length(x))
rnorm(length(x))
hist(length(x))
plot(length(x))

plot(y~x)
lm(y~x)

plot(y~x)
abline(lm(y~x))

N = 50
slope_sim = numeric(N)
slope_sim
for (i in 1:N) {
  y = x + rnorm(length(x))
  y.lm = lm(y~x)
  slope_sim[[i]] = as.vector(y.lm$coefficients[2])
}

slope_sim
plot(density(slope_sim))
mean(slope_sim)
sd(slope_sim)
sd(slope_sim)/sqrt(50)




