## If you pass several vectors to paste0, they are concatenated in a
## vectorized way.
(nth <- paste0(1:12, c("st", "nd", "rd", rep("th", 9))))

## paste works the same, but separates each input with a space.
## Notice that the recycling rules make every input as long as the longest input.
paste(month.abb, "is the", nth, "month of the year.")
paste(month.abb, letters)

## You can change the separator by passing a sep argument
## which can be multiple characters.
paste(month.abb, "is the", nth, "month of the year.", sep = "_*_")

## John Machin (ca 1706) computed pi to over 100 decimal places
## using the Taylor series expansion of the second term of
pi - 4*(4*atan(1/5) - atan(1/239))

# *********************
# this code tests (prepped by ARK) various R code from the Firm Synth model
# *********************
# **********************************
# SimFramework4.r
# **********************************

#load(file=paste(projfolder,"inputs/",firmfn,sep=""))      # IS THIS JUST LOADING THE WORKSPACE??  
load("~/MAG_FirmSynth/03_Firm_Simulation/Model-to-MAG-1-22-2016/Firm_Simulation/inputs/firmseed.rdata")
summary(firmseed)           # IT SEEMS LIKE THE DATA FIRMSEED HAS SOME CONTENTS
View(firmseed)
firmseed=firmseed[firmseed$currtaz>0,]                    # WHERE CAN I SEE CURRTAZ VARIBALE ??
firmseed=firmseed[!is.na(firmseed$age),]                  # WHERE CAN I SEE OTHER VARS LIKE AGE, MOVED, ETC.
firmseed$moved=0
firmseed$death=0
firmseed$movedOut=0
firmseed$ones=1
#####
summary(firmseed)           # IT SEEMS LIKE THE DATA FIRMSEED HAS SOME CONTENTS
#####
View(firmseed)
##########
maxfirmID=max(firmseed$firmID)  # identifies the max. of all firmIDs
maxfirmID

head(firmseed$NAICS6)
View(firmseed$NAICS6)
for(i in 1:4){if(sum(firmseed$NAICS6<100000))
{firmseed$NAICS6[firmseed$NAICS6<100000]=firmseed$NAICS6[firmseed$NAICS6<100000]*10}} # not sure what's happening here??
head(firmseed$NAICS6)
View(firmseed$NAICS6)

# Prep zonal data
library(foreign)
zones=read.dbf("~/Pythprog/Rprog/mag+pag_tazs_se2.dbf")
#zones=prepZones(zones)          # WHERE IS PREPZONES COMING FROM ??  WHERE IS IT DEFINED ??
#Get Zonal Vars for Loc Ch
head(zones)
summary(zones)
str(zones)
colnames(zones)

moveInParams=read.csv("~/Pythprog/Rprog/moveInParams.csv")
str(moveInParams)
View(moveInParams)
mip <- as.character(moveInParams$zonalVars)
summary(mip)
head(mip, 20)
View(mip)
mipmat <- as.matrix(zones[,mip])  # DID NOT WORK; COLUMN NAMES SEEM TO NOT GELLING

head(moveInParams$zonalVars)
View(moveInParams$zonalVars)
moveInZoneMat=as.matrix(zoneID[,as.character(moveInParams$zonalVars)])  # DID NOT WORK; COLUMN NAMES SEEM TO NOT GELLING
moveWithinZoneMat=as.matrix(zones[,as.character(moveWithinParams$zonalVars)]) # DID NOT WORK; COLUMN NAMES SEEM TO NOT GELLING

head(zones)
taz2zoneID =subset(zones, select=c(TAZ,AREA))
head(taz2zoneID)

firmseed = merge(zones,taz2zoneID,by.x="ID",by.y="TAZ", all.x = FALSE)
head(firmseed)
head(zones)
zones = subset(zones, select=-c(RAZ))
head(zones)

###########  SimFunctions.R ######################
createModelVars <- function(firmdata,zonedata){
  # matrix of binaries to get coefficient values for each row?  
  #firmdata$constant=1 
  #calc for companies
  firmdata$isN313 = 0 + (firmdata$NAICS3==313)
  firmdata$isN314 = 0 + (firmdata$NAICS3==314)
  firmdata$isN315 = 0 + (firmdata$NAICS3==315)
  firmdata$isN424 = 0 + (firmdata$NAICS3==424)
  return(firmdata)
}
head(createModelVars)
head(firmdata)  # this has not been populated yet
View(createModelVars)
