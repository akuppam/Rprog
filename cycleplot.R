###Make sure the below packages are installed on your R environment, else first install them 
### using install.packages("packageName")  and then knit this Rmd file

library(lubridate)
library(reshape2)
library(plotly)

library(knitr)


#opts_chunk$set(echo = F, tidy = F, results = 'asis', comment = NA, cache = T)

#Original:
  
#  Visualizing time series data for analyzing any numerical data like revenue, app launches, uninstalls, etc reveals the underlying trend immediately. 
# The graph below captures the normal way of visualizing time series data:
  
setwd("~/Pythprog/Rprog")

dat <- read.csv("dat.csv", stringsAsFactors = F)
summary(dat)

dat$Day <- lubridate::wday(ymd(dat$Date), label=T)
summary(dat$Day)
head(dat$Day)

plot_ly(dat, x= Days, y = Uninstalls, type = "line" ) %>% add_trace( y = fitted(loess(Uninstalls ~ Days)), name = c("smoothened")) %>% layout(showlegend = FALSE)

#The above graph captures the essence of a slight uptrend over the course of 12 weeks but leaves out further details of what's happening during 
# each of those weeks or weekdays.

plot_ly(dat, x = Day, y = Uninstalls, group = Week, type = "point") 

#Each line in the graph represents the time series for each week. Friday has the highest rate of uninstalls which peaks in week 7. 
# Monday, Wednesday, and Thursday have zero uninstalls during each of the 12 weeks which makes it difficult to determine a trend over this period.

#A cycle plot shows both the cycle or trend and the day-of-the-week or the month-of-the-year effect. This retains the strength of both the above plots.
# Let's look at the trend for each day of the week over the same period:
  
{
  ggplot(dat, aes(x = Days,y=Uninstalls, colour = Day)) + geom_point()+
    stat_smooth(size = 0.75) + facet_wrap(~Day) + scale_y_continuous(breaks= c(0,1000,2000,3000,4000))
} %>%
  ggplotly()

############################################
######  USING REAL DATA ###################
###########################################
library(lubridate)
library(reshape2)
library(plotly)

library(knitr)

#opts_chunk$set(echo = F, tidy = F, results = 'asis', comment = NA, cache = T)

setwd("~/MAG_EST-CV Survey/StreetLight/External_for_Client/External_for_Client")

dat <- read.csv("magSLnew5_combined.csv", stringsAsFactors = F)
summary(dat)

dat$Day <- lubridate::wday(ymd(dat$Date.x), label=T)
summary(dat$Day)
head(dat$Day)

plot_ly(dat,x= Date.x,y=travel_time1,type="line") %>% add_trace(y=fitted(loess(travel_time1 ~ Date.x)), name=c("smoothened")) %>% layout(showlegend = FALSE)

#The above graph captures the essence of a slight uptrend over the course of 12 weeks but leaves out further details of what's happening during 
# each of those weeks or weekdays.

plot_ly(dat, x = Date.x, y = travel_time1, group = VehicleWeight.x, type = "point") 

#Each line in the graph represents the time series for each week. Friday has the highest rate of uninstalls which peaks in week 7. 
# Monday, Wednesday, and Thursday have zero uninstalls during each of the 12 weeks which makes it difficult to determine a trend over this period.

#A cycle plot shows both the cycle or trend and the day-of-the-week or the month-of-the-year effect. This retains the strength of both the above plots.
# Let's look at the trend for each day of the week over the same period:

{
ggplot(dat, aes(x = VehicleWeight.x,y=travel_time1, colour = VehicleWeight.x)) + geom_point()+
  stat_smooth(size = 0.75) + facet_wrap(~VehicleWeight.x) + scale_y_continuous(breaks= c(0,1000,2000,3000,4000))
} %>%
  ggplotly()

############################
setwd("~/Pythprog/Rprog")

dat1 <- read.csv("dat1.csv", stringsAsFactors = F)
summary(dat1)

# x should be something that is consecutive (sl no, day no, ID, etc)
# y should be any continous variable
# plot will include all points, trend line
# geom_point is for size, color, shape of the points
# stat_smooth (linear, generalized linear, local smooths like loess)
# facet_wrap

{
ggplot(dat1, aes(x = VehicleID.x,y=tt_xy1,colour = VehicleWeight.x)) + geom_point(alpha=0.25)+
  stat_smooth(size = 0.75) + facet_wrap(~VehicleWeight.x) + scale_y_continuous(breaks= c(0,100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400))
} %>%
  ggplotly()
