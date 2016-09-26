# **************
# functions
# *************
f1 <- function (x)
{
  x * 2 / 4
}

k <- 10
v <- f1(k)
v
v <- f1(10)
v

# *******************
f2 <- function(x)
{
  2 + 2*x + 1*x^2
}
k <- 1
v <- f2(k) # feed the value 'k' into the defined function f2
v
# *******************

v <- c(0, 1, 2, 3, 4)
M <- cbind(c(1,2), c(3,4), c(5,6), c(7,8))

f3 <- function(v,M)
{
  u = c(0,0,0,0,0)
  for(i in 1:length(v))
  {
    u[i]=f1(v[i])
  }
  return(u)
}

Sqv = f3(v)
Sqv

# **********
v <- c(2,4,6,8)

f4 <- function(v)
{
  u = c(0,0,0,0)
  for(i in 1:length(v))
  {
    u[i]=f1(v[i])
  }
  return(u)  
}

v2 <- f4(v)
v2

# ***********************************************
# use function f2 here within another function f5
# f2 =  2 + 2*x + 1*x^2
# **********************************************
a <- c(24, 34, 44, 54)

f5 <- function (a)
{
  q = c(0,0,0,0)
  for(i in 1:length(a))
  {
    q[i] = f2(a[i])
  }
  return (q)
}

# Ulimate goal: apply f5 to 'a', where f5 includes 
# applying f2 to every element inside 'a'

v5 <- f5(a) 
v5

# ************

f6 <- function (n,y=5)
{
  (n * y) + (n / y)
}
v6 <- f6(1)
v6

# ****************
f7 <- function(n,y)
{
  if(missing(y))
  {
    y <- seq(0.05,1,by=0.01)
  }
  return(n^y)
}
v7 <- f7(2)
v7

y <- seq(0.05,1,by=0.01)
y

# ******************
f7 <- function(n,y)
{
  y <- seq(0.05,1,by=0.01)
  return(n^y)
}
v7 <- f7(2)
v7

y <- seq(0.05,1,by=0.01)
y
# ********************

f8 <- function(n,y)
{
  if(missing(y)) y = 0.01;
  if(!y %in% seq(0.05,2,by = 0.001))
    return(n^y)
}
v8 <- f8(1)
v8

# ****************
# tour model code tests
# ***************

n=2
m=3
tab	= matrix(rep(0,n*m),ncol=m)
summary(tab)
head(tab)
str(tab)

# ******************
zonvarnames	= c("taz","tot_emp","ret_emp","frm_emp","min_emp","srv_emp",
                "cns_emp","mnf_emp","whl_emp","tot_pop","tot_hh","lntothh")
#number of zone variables
nzonvar	= length(zonvarnames)
nzonvar
summary(nzonvar)
str(nzonvar)
head(nzonvar)


###################
#Read SED data
###################
get_sed_tab	= function(fname,n,m,cnames){
  recs	<-read.table(fname,header=T,sep=",")
  tab	= matrix(rep(0,n*m),ncol=m)
  for(k in 1:m){
    q		= which(names(recs)==cnames[k])
    tab[,k]	= recs[[q]]
  }
  k1		= which(cnames=="lntothh")
  k2		= which(cnames=="tot_hh")
  tab[,k1]	= log(1+tab[,k2])
  tab
}
summary(recs)
summary(tab)
head(tab)

# *************
# Parameter_Settings.R
# *************
magzones = c(101,3140)
magzones
summary(magzones)
