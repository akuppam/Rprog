# ************************************
# El Paso HH Survey
# ************************************
library(reshape2)
library(dplyr)

setwd("~/El Paso Model/Task 3/HHSurvey/processed survey data/")
ephh <- read.csv("geocoded_HH_survey_record4.csv", header=TRUE)
summary(ephh)

plot(ephh$O_Long, ephh$O_Lat)
plot(ephh$D_Long, ephh$D_Lat)
View(ephh)
od <- ephh$O_TAZ * 10000 + ephh$D_TAZ
ephh1 <- data.frame(ephh, od)
View(ephh1)

amskim <- read.csv("AM_SKIM.csv", header=TRUE)
summary(amskim)
od <- amskim$ORIGIN*10000 + amskim$DESTINATIO
summary(amskim)

#magSLnew2 <- data.frame(magSL, do.call(rbind,magSLnew))

amskim1 <- data.frame(amskim, od)
summary(amskim1)
View(amskim1)

ephh2 <- merge(ephh1, amskim1, by="od")
summary(ephh2)
View(ephh2)
str(ephh2)
colnames(ephh2)

write.csv(ephh2, "ephh2.csv", row.names = FALSE)
save(ephh2, file = "ephh2.Rda")


# **************************
# R pirate plots
# *************************
setwd("~/El Paso Model/Task 3/HHSurvey/processed survey data/")
load("ephh2.Rda")

library("devtools")
library("yarrr")

jpeg(file = "TT Dist by Purpose.jpg")
pirateplot(formula = ephh2$AB_TT_AM__ ~ ephh2$trip_purpose,
           data = ephh2[which(ephh2$AB_TT_AM__ > 0),],
           pal = "google",
           bean.o = .3,
           back.col = transparent("blue",.9),
           line.fun = mean,
           line.lwd = 8,
           gl.col = gray(.5),
           bar.o = (.8),
           bar.border.col = gray(.9),
           ylim = c(0,70),
           xlab = "Trip Purpose",
           ylab = "AM Trip Length (minutes)",
           main = "Trip Len by Purpose")
dev.off()

library("ggplot2")

jpeg(file = "TT Dist by Purpose 1.jpg")
ggplot(data=ephh2, aes(x=trip_purpose, y=AB_TT_AM__, group=trip_purpose, colour=trip_purpose)) +
  geom_line() +
  geom_point()
dev.off()

# *************
# TLFDs
# ************
binwidth <- 1.0     # 1-minute interval
breaks1 <- seq(floor(min(ephh2$AB_TT_AM__)), ceiling(max(ephh2$AB_TT_AM__)), binwidth)
breaks2 <- seq(floor(0.1), ceiling(75.0), binwidth)

hist(ephh2$AB_TT_AM__, breaks1)

avgtt <- mean(ephh2$AB_TT_AM__)
sdtt <- sd(ephh2$AB_TT_AM__)

# histogram + normal curve (peak at the mean value)
jpeg(file = "Trip Length Histogram with Density Curve - all purposes.jpg")
ggplot(ephh2, aes(AB_TT_AM__)) +
  geom_histogram(aes(y = ..density..)) +
  geom_vline(xintercept=mean(ephh2$AB_TT_AM__), color="blue") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(ephh2$AB_TT_AM__), sd = sd(ephh2$AB_TT_AM__)),
                lwd = 2, 
                col = 'red')
dev.off()

jpeg(file = "Trip Length Histogram with Frequency Curve - all purposes.jpg")
ggplot(ephh2, aes(AB_TT_AM__)) +
  geom_histogram(aes(y = ..count..), binwidth = 1.0, col = 'black') + 
  geom_freqpoly(aes(y = ..count..), binwidth = 1.0, lwd = 1.2, col = 'red') +
  geom_vline(xintercept=mean(ephh2$AB_TT_AM__), lwd = 1.2, col="blue")
# geom_label(label.size = 0.25)
#  geom_text(aes(label = "tlfd"), show.legend = NA)

#    legend(x = "topright",
#         c("Density plot", "Frequency Curve", "Mean"),
#         col = c("black", "red", "blue"),
#         lwd = c(2, 2, 2))
dev.off()

library(lattice)
densityplot(~ AB_TT_AM__, data = ephh2)

library(ggplot2)
par(mfrow = c(2, 2))
qplot(AB_TT_AM__, data=ephh2, geom ="histogram", binwidth = 1.0)
qplot(AB_TT_AM__, data=ephh2, geom ="density")

# ********************************************************
# weighted summary of trips by purpose
str(ephh2)
summary(ephh2$trip_purpose)
summary(rep(ephh2$trip_purpose, ephh2$Adj_weight_prox_gen_age))

#library(Hmisc)
#wtd.mean(ephh2$trip_purpose, weights=ephh2$Adj_weight_prox_gen_age, normwt=TRUE, na.rm=TRUE)

# ********************************************************
# ********************************************************
# ********************************************************
# no need to run the following block of code

ephh2age1 <- subset(ephh2, age.group1=="1", na.rm=TRUE)
qplot(AB_TT_AM__, data=ephh2age1, geom ="histogram", binwidth = 1.0, main = "age 1")
mean(ephh2age1$AB_TT_AM__)

ephh2age1 <- rep(subset(ephh2, age.group1=1, na.rm=TRUE), ephh2$Adj_weight_prox_gen_age)
qplot(AB_TT_AM__, data=ephh2age1, geom ="histogram", binwidth = 1.0, main = "age 1")
mean(ephh2age1$AB_TT_AM__)

ephh2age1 <- subset(ephh2, rep((age.group1=1), ephh2$Adj_weight_prox_gen_age))
qplot(AB_TT_AM__, data=ephh2age1, geom ="histogram", binwidth = 1.0, main = "age 1")
mean(ephh2age1$AB_TT_AM__)

ephh2age1 <- subset(rep(ephh2, ephh2$age.group1=="1", na.rm=TRUE), ephh2$Adj_weight_prox_gen_age)
qplot(rep(AB_TT_AM__, ephh2$Adj_weight_prox_gen_age, na.rm=TRUE), data=ephh2age1, geom ="histogram", binwidth = 1.0, main = "age 1")
mean(ephh2age1$AB_TT_AM__)

tp=as.numeric(ephh2$trip_purpose)
summary(tp)
ephh2age1 <- subset(rep(ephh2, ephh2$age.group1=="1", na.rm=TRUE), ephh2$Adj_weight_prox_gen_age)
ephh2hbw <- rep(subset(ephh2, tp=1, na.rm=TRUE), ephh2$Adj_weight_prox_gen_age)
jpeg(file = "hbw.jpg")
qplot(AB_TT_AM__, data=ephh2hbw, geom ="histogram", binwidth = 1.0, main = "hbw")
dev.off()
mean(ephh2hbw$AB_TT_AM__)

# ********************************************************
# ********************************************************
# ********************************************************
# *******************
# histograms of AM trip lengths by purpose
# also showing normal density curves
# these are NOT weighted summaries
# *******************
par(mfrow = c(3, 3))

repa <- rep(ephh2$AB_TT_AM__, ephh2$Adj_weight_prox_gen_age)
repb <- rep(ephh2$tp, ephh2$Adj_weight_prox_gen_age)

# 1	        2	        3	        4	        5   6	      7
# hbnw_col	hbnw_oth	hbnw_ret	hbnw_sch	hbw	nhb_oth	nhb_wrk


ephh2hbw <- subset(ephh2, tp=5)
repc <- rep(ephh2hbw$AB_TT_AM__, ephh2hbw$Adj_weight_prox_gen_age)
summary(repc)
lenc <- length(repc)
lenc

jpeg(file = "hbw1.jpg")
qplot(ephh2hbw$repc, data=ephh2hbw, geom ="histogram", binwidth = 1.0, main = "hbw")
dev.off()
mean(ephh2hbw$AB_TT_AM__)

# ********************

ephh2hbw <- subset(ephh2, trip_purpose=="hbw")
jpeg(file = "hbw.jpg")
qplot(AB_TT_AM__, data=ephh2hbw, geom ="histogram", binwidth = 1.0, main = "hbw")
dev.off()
mean(ephh2hbw$AB_TT_AM__)

ephh2hbnwret <- subset(ephh2, trip_purpose=="hbnw_ret")
jpeg(file = "hbnwret.jpg")
qplot(AB_TT_AM__, data=ephh2hbnwret, geom ="histogram", binwidth = 1.0, main = "hbnwret")
dev.off()
mean(ephh2hbnwret$AB_TT_AM__)

ephh2hbnwoth <- subset(ephh2, trip_purpose=="hbnw_oth")
jpeg(file = "hbnwoth.jpg")
qplot(AB_TT_AM__, data=ephh2hbnwoth, geom ="histogram", binwidth = 1.0, main = "hbnwoth")
dev.off()
mean(ephh2hbnwoth$AB_TT_AM__)

ephh2hbnwsch <- subset(ephh2, trip_purpose=="hbnw_sch")
jpeg(file = "hbnwsch.jpg")
qplot(AB_TT_AM__, data=ephh2hbnwsch, geom ="histogram", binwidth = 1.0, main = "hbnwsch")
dev.off()
mean(ephh2hbnwsch$AB_TT_AM__)

ephh2hbnwcol <- subset(ephh2, trip_purpose=="hbnw_college")
jpeg(file = "hbnwcol.jpg")
qplot(AB_TT_AM__, data=ephh2hbnwcol, geom ="histogram", binwidth = 1.0, main = "hbnwcol")
dev.off()
mean(ephh2hbnwcol$AB_TT_AM__)

ephh2nhbwrk <- subset(ephh2, trip_purpose=="nhb_wrk")
jpeg(file = "nhbwrk.jpg")
qplot(AB_TT_AM__, data=ephh2nhbwrk, geom ="histogram", binwidth = 1.0, main = "nhbwrk")
dev.off()
mean(ephh2nhbwrk$AB_TT_AM__)

ephh2nhboth <- subset(ephh2, trip_purpose=="nhb_oth")
jpeg(file = "nhboth.jpg")
qplot(AB_TT_AM__, data=ephh2nhboth, geom ="histogram", binwidth = 1.0, main = "nhboth")
dev.off()
mean(ephh2nhboth$AB_TT_AM__)

# *********************************
# frequency tabulations
# *********************************

ephh2$tp=as.numeric(ephh2$trip_purpose)
summary(ephh2$tp)

repa <- rep(ephh2$AB_TT_AM__, ephh2$Adj_weight_prox_gen_age)
repb <- rep(ephh2$tp, ephh2$Adj_weight_prox_gen_age)

lenrepa=length(repa)
lenrepb=length(repb)
lenrepa
lenrepb

ephhtables <- table(repa, repb)
View(ephhtables)
write.csv(ephhtables, "ephhtables.csv")
summary(ephhtables)
str(ephhtables)

ephhtables1 <- read.csv("ephhtables.csv", header=TRUE)
str(ephhtables1)
View(ephhtables1) # this tables has all the 7 purposes; 
# need to split them up into 7 diff tables, and do the histogram individually

# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# the following block of code plots shows actual count
# it still differs from the SPSS plot
# but can be done for all other purposes too
ephhtables1col <- data.frame(ephhtables1$X, ephhtables1$X1)
ephhtables2col <- subset(ephhtables1col, ephhtables1.X1>0)

View(ephhtables2col)
str(ephhtables2col)

hbcoltlfd <- rep(ephhtables2col$ephhtables1.X, ephhtables2col$ephhtables1.X1)
length(hbcoltlfd)
write.csv(hbcoltlfd, "hbcoltlfd.csv")
summary(hbcoltlfd)

hbcoltlfd1 <- read.csv("hbcoltlfd.csv", header=TRUE)
str(hbcoltlfd1)
View(hbcoltlfd1)

ggplot(hbcoltlfd1, aes(x)) +
  geom_histogram(aes(y = ..count..), binwidth = 0.75, col = 'black') + 
#  geom_freqpoly(aes(y = ..count..), binwidth = 1.0, lwd = 1.2, col = 'red') +
  geom_vline(xintercept=mean(hbcoltlfd1$x), lwd = 1.2, col="blue")

# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# EXPLORE WHAT BINWIDTH IS....IT IS INCREASING/DECRESING THE COUNT ON Y-AXIS

# ----- the below code gives normal dist but NOT the count ----------

ggplot(hbcoltlfd1, aes(x)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.75, col = 'black') +
  geom_vline(xintercept=mean(hbcoltlfd1$x), lwd = 1.2, col="blue") + 
  stat_function(fun = dnorm, 
                args = list(mean = mean(hbcoltlfd1$x), sd = sd(hbcoltlfd1$x)),
                lwd = 2, 
                col = 'red')
# ------------

getwd()
