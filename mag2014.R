setwd("~/MAG_TruckTour/Models/Estimation/Final_Models_013016")


library(dplyr)
library(ggplot2)

mag2014 <- read.csv("MAG_STOPS-F.csv", header = TRUE)

#row.names(initial) 							# writing row names to the console (also counting no of rows)
colnames(mag2014) 								# writing variable or field names to the console
summary(mag2014)
str(mag2014)

# ******************************************************************

qplot(stpxcoor, stpycoor, data=mag2014)

#####################

# need to have a categorical var as the first var
# need to have two continous vars after the categorical var
interaction.plot(mag2014$trstps, mag2014$stppurp, mag2014$trcomp)
#plot.design(mag2014)

library(rgl)
plot3d(mag2014$trstps, mag2014$stppurp, mag2014$trcomp, col = "red", size = 3)

library(rgl)
library(plot3D)
##############################################################
#######  ADDING COLORS TO THE 3D PLOT ###################
#############################################################
c = mag2014$stppurp
c = cut(c, breaks=64)
cols = rainbow(64)[as.numeric(c)]

plot3d(mag2014$trstps, mag2014$stppurp, mag2014$trcomp, col=cols)
#################################################################
#############################################################
c = mag2014$trcomp
c = cut(c, breaks=64)
cols = rainbow(64)[as.numeric(c)]

plot3d(mag2014$trstps, mag2014$stppurp, mag2014$trcomp, col=cols)
#################################################################
#############################################################
c = mag2014$trstps
c = cut(c, breaks=64)
cols = rainbow(64)[as.numeric(c)]

plot3d(mag2014$trstps, mag2014$stppurp, mag2014$trcomp, col=cols)
#################################################################
#############################################################
c = mag2014$stppurp2
c = cut(c, breaks=64)
cols = rainbow(64)[as.numeric(c)]

plot3d(mag2014$trstps, mag2014$stppurp2, mag2014$trcomp, col=cols)
#############################################################
#############################################################
#############################################################
###  NEW DATA SET WITH RE-CODED PURPOSES           ##########
#############################################################
#############################################################
#############################################################

mag2014new <- read.csv("MAG_STOPS-F-newtrpurp.csv", header = TRUE)

#row.names(initial) 							# writing row names to the console (also counting no of rows)
colnames(mag2014new) 								# writing variable or field names to the console
summary(mag2014new)
str(mag2014new)

library(rgl)
######################################################################
c = mag2014new$newstppurp
c = cut(c, breaks=64)
cols = rainbow(64)[as.numeric(c)]

plot3d(mag2014new$trstps, mag2014new$newstppurp, mag2014new$trcomp, col=cols)
#######################################################################
mag2014new1 <- data.frame(mag2014new$trstps, mag2014new$newtrpurp, mag2014new$trcomp, mag2014new$newstppurp, mag2014new$tt_1p)
head(mag2014new1)
pairs(mag2014new1)
#########################
mag2014new1 <- data.frame(mag2014new$trstps, mag2014new$newtrpurp, mag2014new$newstppurp)
head(mag2014new1)
pairs(mag2014new1)
###########################
mag2014new1 <- data.frame(mag2014new$trstps, mag2014new$newtrpurp, mag2014new$trdtim, mag2014new$newstppurp)
head(mag2014new1)
pairs(mag2014new1)
#########################
mag2014new1 <- data.frame(mag2014new$trstps, mag2014new$newtrpurp, mag2014new$tt_1p, mag2014new$newstppurp)
head(mag2014new1)
pairs(mag2014new1)
########################################
#######################################
##  CHAPTER 6 #######################
####################################

qplot(tt_1p, newtrpurp, data=mag2014new)
qplot(tt_1p, trdtim, data=mag2014new)
###################################
timemodel <- lm(tt_1p ~ trdtim + trstps, data = mag2014new)
summary(timemodel)
write(predict(timemodel), file = "predicttimemodel.csv")

#######################################
##  CHAPTER 7 #######################
####################################

#persp(x = mag2014new$trstps, y = mag2014new$newtrpurp, z = mag2014new$tt_1p)
lines3D(mag2014new$trstps, mag2014new$newtrpurp, mag2014new$tt_1p)

#######################################
##  CHAPTER 8 #######################
####################################

library("rpart")
timemodel_rpart <- rpart(tt_1p ~ trdtim + trstps, data = mag2014new)
plot(timemodel_rpart, uniform = TRUE, margin = 0.1, branch = 0.5, compress = TRUE)
text(timemodel_rpart)

#######################################
##  CHAPTER 12 #######################
####################################

library("rmeta")
magmeta <- meta.MH(mag2014new1[["trcomp"]], mag2014new1[["newtrpurp"]])
plot(magmeta, ylab="")

