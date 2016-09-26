setwd("~/ARK/BD/MAG/FreightModel/TO's/FreightModelValid")

library(plyr)
library(dplyr)
library(ggplot2)
library(mcsm)
library(descr)
library(lattice)
library(reshape)

scoutputs <- read.csv("CommodityFlows_firmlevel.csv", header = TRUE)

colnames(scoutputs) 								# writing variable or field names to the console
summary(scoutputs)
str(scoutputs)

# *************************
#scoutputs$dnaics <- revalue(data$sex, c("M"="1", "F"="2"))
#as.numeric(levels(scoutputs$dest_naics))[scoutputs$dest_naics]
#scoutputs$dnaics <- mapvalues(scoutputs$dest_naics, from = c(">=420000 & <=429999"), to = c("42"))
#scoutputs$dnaics <- revalue(scoutputs$dest_naics, c(">=420000 & <=429999"="1"))
#scoutputs$dnaics[scoutputs$dest_naics>=420000 & scoutputs$dest_naics<=429999] <- "42"

save(scoutputs, file = "sco.Rda")

setwd("~/ARK/BD/MAG/FreightModel/TO's/FreightModelValid")
load("sco.Rda")
summary(scoutputs)

#scoutputs$onaics[scoutputs$orig_naics>=420000 & scoutputs$orig_naics<=429999] <- 42
#as.numeric(levels(scoutputs$onaics))[scoutputs$onaics]

#scoutputs$dnaics1[scoutputs$dnaics>=420000 & scoutputs$dnaics<=429999] = 42

#mydata$agecat[age > 45 & age <= 75] <- "Middle Aged"

# *****************
scoutputs$dnaics<-as.numeric(as.character(scoutputs$dest_naics))
# *****************
summary(scoutputs)
#mydata$Agecat1<-cut(mydata$Age, c(0,5,10,15,20,25,30))
#mydata$Agecat4<-cut(mydata$Age, seq(0,30,5), right=FALSE, labels=c(1:6)) 

scoutputs$dnaics1<-cut(scoutputs$dnaics, c(110000,
                                                210000,
                                                220000,
                                                230000,
                                                310000,
                                                320000,
                                                330000,
                                                420000,
                                                440000,
                                                450000,
                                                480000,
                                                490000,
                                                510000,
                                                520000,
                                                530000,
                                                540000,
                                                550000,
                                                560000,
                                                610000,
                                                620000,
                                                710000,
                                                720000,
                                                810000,
                                                920000))


summary(scoutputs)
str(scoutputs)

scoutputs$dnaics2<-as.numeric(as.character(scoutputs$dnaics1))
summary(scoutputs)
str(scoutputs)

save(scoutputs, file = "sco.Rda")

setwd("~/ARK/BD/MAG/FreightModel/TO's/FreightModelValid")
load("sco.Rda")
summary(scoutputs)

# **************************
# R pirate plots
# *************************
library("devtools")
library("yarrr")

pirateplot(formula = Truck_Medium ~ dest_naics,
           data = scoutputs,
           line.fun = mean,
           line.lwd = 8,
           gl.col = gray(.9),
           bar.border.col = gray(.9),
           xlab = "DEST NAICS",
           ylab = "Truck - Medium Shipment",
           main = "Trucks destined to NAICS 1")

#df[which(df$prop>0),]

pirateplot(formula = Truck_Medium ~ dnaics,
           data = scoutputs,
           line.fun = mean,
           line.lwd = 8,
           gl.col = gray(.9),
           bar.border.col = gray(.9),
           xlab = "DEST NAICS",
           ylab = "Truck - Medium Shipment",
           main = "Trucks destined to NAICS 2")

pirateplot(formula = Truck_Medium ~ dnaics1,
           data = scoutputs,
           line.fun = mean,
           line.lwd = 8,
           gl.col = gray(.9),
           bar.border.col = gray(.9),
           xlab = "DEST NAICS",
           ylab = "Truck - Medium Shipment",
           main = "Trucks destined to NAICS 3")

# ********************************
library("beanplot") 
par(mfrow = c(1, 2), mai = c(0.5, 0.5, 0.5, 0.1)) 
mu <- 2 
si <- 0.6 
c <- 500 
bimodal <- c(rnorm(c/2, -mu, si), rnorm(c/2, mu, si)) 
uniform <- runif(c, -4, 4) 
normal <- rnorm(c, 0, 1.5) 
ylim <- c(-7, 7) 
boxplot(scoutputs$Truck_Medium, bimodal, uniform, normal, ylim = ylim, main = "boxplot") 
beanplot(scoutputs$Truck_Medium, bimodal, uniform, normal, ylim = ylim, main = "beanplot")

beanplot(Truck_Medium ~ dnaics1, 
         data=scoutputs, 
         xlab = "DEST NAICS", 
         ylab = "Truck Medium", 
         main = "beanplot")

beanplot(scoutputs$Truck_Medium, 
         xlab = "DEST NAICS", 
         ylab = "Truck Medium", 
         main = "beanplot")

# ******************
# bean test
# *****************

library("devtools")
library("yarrr")
library("beanplot") 

setwd("~/ARK/BD/MAG/FreightModel/TO's/FreightModelValid")

btest <- read.csv("beantest.csv", header = TRUE)
summary(btest)
str(btest)

pirateplot(formula = ht ~ abc,
           data = btest,
           line.fun = mean,
           line.lwd = 8,
           gl.col = gray(.9),
           bar.border.col = gray(.9),
           xlab = "abc",
           ylab = "ht",
           main = "abc x ht")

# ****************
par(mfrow = c(2, 2))

pirateplot(formula = wt ~ abc,
           data = btest,
           line.fun = mean,
           line.lwd = 8,
           gl.col = gray(.9),
           bar.border.col = red(.9),
           xlab = "abc",
           ylab = "wt",
           main = "abc x wt")

pirateplot(formula = wt ~ abc,
           data = btest,
           pal = "google",
           bean.o = .4,
           back.col = transparent("blue",.9),
           line.fun = mean,
           line.lwd = 8,
           gl.col = gray(.5),
           bar.o = (.5),
           bar.border.col = gray(.9),
           xlab = "abc",
           ylab = "wt",
           main = "abc x wt")

pirateplot(formula = wt ~ abc,
           data = btest,
           line.fun = mean,
           line.lwd = 28,
           gl.col = gray(.1),
           bar.border.col = gray(.1),
           xlab = "abc",
           ylab = "wt",
           main = "abc x wt")

# *****************
beanplot(ht ~ abc, 
         data=btest, 
         xlab = "abc", 
         ylab = "ht", 
         main = "beanplot - abc x ht")

par(mfrow = c(1, 2), mai = c(0.5, 0.5, 0.5, 0.1)) 
mu <- 2 
si <- 0.6 
c <- 500 
bimodal <- c(rnorm(c/2, -mu, si), rnorm(c/2, mu, si)) 
uniform <- runif(c, -4, 4) 
normal <- rnorm(c, 0, 1.5) 
ylim <- c(-7, 7) 

beanplot(btest$ht,
         bimodal,
         uniform,
         normal,
         xlab = "abc", 
         ylab = "ht", 
         ylim = ylim,
         main = "beanplot")
