# ------------------------------------
# MAG Freight Model Plots
# ------------------------------------

setwd("~/MAG_LM_TruckTour/Model/Medium Trucks Model Output")
medtour <- read.csv("tours_output.csv", sep= ",")
summary(medtour)
str(medtour)

library(dplyr)
library(ggplot2)
library(devtools)
library(yarrr)
library(beanplot)

jpeg(file = "~/MAG_LM_TruckTour/Model/Medium Trucks Model Output/purpstop-bean.jpeg")
beanplot(medtour$tourstops ~ medtour$tourpurp)
dev.off()

jpeg(file = "~/MAG_LM_TruckTour/Model/Medium Trucks Model Output/purpstop-pirate.jpeg")
pirateplot(formula = medtour$tourstops ~ medtour$tourpurp,
           data = medtour,
           pal = "google",
           bean.o = .4,
           back.col = transparent("blue",.9),
           line.fun = mean,
           line.lwd = 8,
           gl.col = gray(.5),
           bar.o = (.5),
           bar.border.col = gray(.9),
           xlab = "Tour Purpose",
           ylab = "Tour Stops",
           main = "No. of Stops by Tour Type")
dev.off()

#library(psych)
#pairs.panels(medtour)

View(medtour)

library(splines)
jpeg(file = "~/MAG_LM_TruckTour/Model/Medium Trucks Model Output/stops-hist.jpeg")
qplot(tourstops, data = medtour, geom = "histogram", binwidth = 1, xlim = c(0,3))
dev.off()

jpeg(file = "~/MAG_LM_TruckTour/Model/Medium Trucks Model Output/tourpurp-dens.jpeg")
qplot(medtour$tourpurp, geom = "density", colour = medtour$tourpurp)
dev.off()

jpeg(file = "~/MAG_LM_TruckTour/Model/Medium Trucks Model Output/tourpurp-hist.jpeg")
qplot(medtour$tourpurp, geom = "histogram")
dev.off()

jpeg(file = "~/MAG_LM_TruckTour/Model/Medium Trucks Model Output/purpstops.jpeg")
qplot(medtour$tourpurp, medtour$tourstops,
      geom = c("point"),
      xlab = "Tour Purpose",
      ylab = "No. of Stops",
      size = factor(medtour$tourstops),
      color = factor(medtour$tourcomp))
#+ scale_size_manual(values = c(3, 7)) 
#+ scale_colour_manual(values = c("red", "blue")
dev.off()

jpeg(file = "~/MAG_LM_TruckTour/Model/Medium Trucks Model Output/tourcomp-dens.jpeg")
qplot(medtour$tourpurp,
      geom = c("density"),
      xlab = "Tour Purpose",
      ylab = "Density",
#      size = factor(medtour$tourstops),
      color = factor(medtour$tourcomp))
#+ scale_size_manual(values = c(3, 7)) 
#+ scale_colour_manual(values = c("red", "blue")
dev.off()

jpeg(file = "~/MAG_LM_TruckTour/Model/Medium Trucks Model Output/tourcomp-hist.jpeg")
qplot(medtour$tourpurp,
      geom = c("histogram"),
      xlab = "Tour Purpose",
      ylab = "Density",
      #      size = factor(medtour$tourstops),
      color = factor(medtour$tourcomp))
#+ scale_size_manual(values = c(3, 7)) 
#+ scale_colour_manual(values = c("red", "blue"))
dev.off()

jpeg(file = "~/MAG_LM_TruckTour/Model/Medium Trucks Model Output/pairs.jpeg")
pairs(medtour[1:5])
dev.off()
