setwd("~/WisDOT/ATRI")

library(dplyr)
library(ggplot2)

initial <- read.csv("WisDOT_TruckTrips_Draft_5min_revisedTruckIDs - ark.csv", header = TRUE)

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

# writew out the new data file into to the same directory
write.csv(initialNew2, file = "~/WisDOT/ATRI/WI-ATRI-new.csv", append = FALSE, sep = " ")

# ******************************************************************

# code below was the several tries to split the column (unsuccessfully) but has some new and useful functions
out <- strsplit(as.character(initial$OriginDateStamp),'  ')
initial2 <- data.frame(initial, do.call(rbind,out))
colnames(initial2)
summary(initial2)

write.csv(initial2, file = "~/WisDOT/ATRI/new.csv", append = FALSE, sep = " ")

library(splitstackshape)
initial3 <- cSplit(initial, "OriginDateStamp", "  ", fixed = FALSE)
colnames(initial3)
summary(initial3)

initial4 <- as.data.frame(do.call(rbind, strsplit(as.character(initial$OriginDateStamp), split="  ", perl=TRUE)))
colnames(initial4)
summary(initial4)

initial5 <- setDT(initial)[, c('date', 'time') := tstrsplit(OriginDateStamp, "   ",  perl=TRUE, type.convert=TRUE)]
colnames(initial5)
summary(initial5)
head(initial5)

#ID1 <- c("Indiv01A", "Indiv01B", "Indiv02A", "Indiv02B", "Speci03A", "Speci03B")
res <- strsplit(initial$OriginDateStamp, "(?<=.{11})", perl = TRUE)
do.call(rbind, res)

head(initial)
initial6 <- substr(initial$OriginDateStamp, 1, 2)
head(initial6)

initial7 <- sub("(^[A-Z]{1}).*", initial$OriginDateStamp)
head(initial7)
colnames(initial7)

#sst <- strsplit(initial[,"OriginDateStamp"], "")
#additional.cols<-rbind.fill.matrix(lapply(sst, rbind))
#colnames(additional.cols)<-c(paste("OriginDateStamp",sequence(ncol(additional.cols)),sep=""))

sst <- strsplit(as.character(initial$OriginDateStamp),'  ')
additional.cols <- data.frame(lapply(sst, rbind))
colnames(additional.cols)<-c(paste("OriginDateStamp",sequence(ncol(additional.cols)),sep=""))

head(additional.cols)
summary(additional.cols)

