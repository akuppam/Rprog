# commands from 'reshape2' library package
library(reshape2)
library(dplyr)
library(MASS)

# without loading dplyr, tbl_df will not work
# basics stats####
cw <- tbl_df(ChickWeight)
colnames(cw)
summary(cw) # basics stats END ####

names(cw) <- tolower(names(cw))
chick_m <- melt(cw, id=2:4, na.rm = TRUE)
dcast(chick_m, time~variable, mean)

firm_births<-function(birth_rates,nsim){
  birth_rates=birth_rates;nsim=nsim
  firm_rates<-matrix(0,nrow=nrow(birth_rates),ncol=nsim)
  ## Birth rates being stored in sample based on the number of simulation (or number of years)
  for (i in 1:nrow(firm_rates)){
    firm_rates[i,]=rnbinom(nsim,mu=birth_rates[i,2],size=birth_rates[i,3])
  }
  rownames(firm_rates)<-birth_rates$NAICS
  colnames(firm_rates)<-paste("SimYear",1:nsim,sep="")
  return(firm_rates)
}
  ## ## Employment distribution at birth is assumed to be log-normal with parameters
  ## mean 0.6892 and sd 0.79708
  ## The likelihood of the new born being a Subsidiary is 3%
#### FIRM BIRTH FUNCTION END ####
colnames(firm_rates)
summary(firm_rates)
print (firm_rates)

## Alternative parametrization
x1 <- rnbinom(500, mu = 4, size = 1)
x2 <- rnbinom(500, mu = 4, size = 10)
x3 <- rnbinom(500, mu = 4, size = 100)
h1 <- hist(x1, breaks = 20, plot = FALSE)
h2 <- hist(x2, breaks = h1$breaks, plot = FALSE)
h3 <- hist(x3, breaks = h1$breaks, plot = FALSE)
barplot(rbind(h1$counts, h2$counts, h3$counts),
        beside = TRUE, col = c("red","blue","cyan"),
        names.arg = round(h1$breaks[-length(h1$breaks)]))
print(x1)
print(x2)
print(x3)

