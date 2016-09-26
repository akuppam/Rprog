source("funcsin.R")

k <- 2
v <- f1(k)
v
# *******
k1 <- 0
v1 <- f2(k1)
v1

# ******************
# finding and listing functions in another file
# *****************

sapply(list.files(pattern="[.]R$", path="C:/Users/akuppam/Documents/Pythprog/Rprog/", full.names=TRUE), source)

if(exists("f1", mode = "function"))
   source("funcsin.R")
   

