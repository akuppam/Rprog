## optimize examples

f = function(x,a=4) sin(a*x) + exp(x) + x^2
lo=-4; hi=3
curve(f(x),lo,hi,n=1e3)
steps <- c()
fsave = function(x) {steps <<- c(steps,x); f(x)} 
opt = optimize(fsave,c(lo,hi))
pts = cbind(steps,f(steps))
it=nrow(pts)
lines(pts,col='green')
arrows(pts[-it,1], pts[-it,2], pts[-1,1], pts[-1,2], angle=10, col="red")
text(pts[,1],pts[,2])




#### optim examples (adapted from code in optim help)

fr <- function(x) {   ## Rosenbrock Banana function
   x1 <- x[1]
   x2 <- x[2]
   100 * (x2 - x1 * x1)^2 + (1 - x1)^2
}
grr <- function(x) { ## Gradient of 'fr'
   x1 <- x[1]
   x2 <- x[2]
   c(-400 * x1 * (x2 - x1 * x1) - 2 * (1 - x1),
      200 *      (x2 - x1 * x1))
}
hrr <- function(x)  { ## Analytical hessian
  x1 <- x[1]
  x2 <- x[2]
  hess = matrix(NA,2,2)
  hess[1,1] <- -400 * (x2 - 3*x1^2) + 2
  hess[1,2] <- -400 * x1
  hess[2,1] <- -400 * x1
  hess[2,2] <- 200
  hess
}


# plot this function
rng=1; x<-y<-seq(-rng,rng,length.out=40);
z=outer(x,y,function(x,y) {apply(cbind(x,y),1,fr)});
n=length(x); zfct=(z[-1,-1] + z[-1,-n] + z[-n,-1] + z[-n,-n])/4
persp(x,y,z,theta=150,phi=25,border=3,col=c('white','red')[1+(zfct<4)],lwd=.3)

# true global minimum at (1,1), value 0
# N-M takes almost 200 function draws
optim(c(-1.2,1), fr)
# BFGS with gradient is 110 function draws, 43 gradient, but ends up more accurate
optim(c(-1.2,1), fr, grr, method = "BFGS")
# BFGS without gradient is less accurate, does 38 draws of numerical gradient (not including calculation of hessian)
optim(c(-1.2,1), fr, NULL, method = "BFGS", hessian = TRUE)
# simulated annealing
optim(c(-1.2,1), fr, method = "SANN")
## These algorithms do not converge in the default number of steps
# conjugate gradient methods
optim(c(-1.2,1), fr, grr, method = "CG")
optim(c(-1.2,1), fr, grr, method = "CG", control=list(type=2))
# low-memory bfgs
optim(c(-1.2,1), fr, grr, method = "L-BFGS-B")



# constrained optimization
flb <- function(x)
   { p <- length(x); sum(c(1, rep(4, p-1)) * (x - c(1, x[-p])^2)^2) }
## 25-dimensional box constrained
optim(rep(3, 25), flb, NULL, method = "L-BFGS-B",
     lower=rep(2, 25), upper=rep(4, 25)) # par[24] is *not* at boundary

## "wild" function , global minimum at about -15.81515
fw <- function (x) 10*sin(0.3*x)*sin(1.3*x^2) + 0.00001*x^4 + 0.2*x+80
plot(fw, -50, 50, n=1000, main = "optim() minimising 'wild function'")

res <- optim(50, fw, method="SANN",
            control=list(maxit=20000, temp=20, parscale=20))
res
points(res$par, res$value, pch = 8, col = "green", cex = 2)
## Now improve locally {typically only by a small bit}:
(r2 <- optim(res$par, fw, method="BFGS"))
points(r2$par, r2$value, pch = 8, col = "red", cex = 2)



## Combinatorial optimization: Traveling salesman problem
eurodistmat <- as.matrix(eurodist)

distance <- function(sq) {  # Target function
   sq2 <- embed(sq, 2)
   sum(eurodistmat[cbind(sq2[,2],sq2[,1])])
}

genseq <- function(sq) {  # Generate new candidate sequence
   idx <- seq(2, NROW(eurodistmat)-1)
   changepoints <- sample(idx, size=2, replace=FALSE)
   tmp <- sq[changepoints[1]]
   sq[changepoints[1]] <- sq[changepoints[2]]
   sq[changepoints[2]] <- tmp
   sq
}
sq <- c(1:nrow(eurodistmat), 1)  # Initial sequence: alphabetic
distance(sq)
# rotate for conventional orientation
loc <- -cmdscale(eurodist, add=TRUE)$points
x <- loc[,1]; y <- loc[,2]
s <- seq_len(nrow(eurodistmat))
tspinit <- loc[sq,]

plot(x, y, type="n", asp=1, xlab="", ylab="",
    main="initial solution of traveling salesman problem", axes = FALSE)
arrows(tspinit[s,1], tspinit[s,2], tspinit[s+1,1], tspinit[s+1,2],
      angle=10, col="green")
text(x, y, labels(eurodist), cex=0.8)

set.seed(123) # chosen to get a good soln relatively quickly
res <- optim(sq, distance, genseq, method = "SANN",
            control = list(maxit = 30000, temp = 2000, trace = TRUE,
                           REPORT = 500))
res  # Near optimum distance around 12842

tspres <- loc[res$par,]
plot(x, y, type="n", asp=1, xlab="", ylab="",
    main="optim() 'solving' traveling salesman problem", axes = FALSE)
arrows(tspres[s,1], tspres[s,2], tspres[s+1,1], tspres[s+1,2],
      angle=10, col="red")
text(x, y, labels(eurodist), cex=0.8)


# function composing function
`%c%` = function(f,g) function(...) f(g(...))

##### maxLik package
# main benefit is additional algorithms like BHHH
# can also use Newton-Raphson if analytical hessian is simple
library(maxLik)

# N-R with analytical gradients
maxLik(start=c(-1.2,1), `-` %c% fr, `-` %c% grr, `-` %c% hrr)
maxLik(start=c(-1.2,1), `-` %c% fr)

# has convenience functions to extract coefficients, inverse hessians
opt = maxLik(start=c(-1.2,1), `-` %c% fr)
coef(opt)
vcov(opt)
# compare numerical vcov estimate with analytical inverse hessian
solve(hrr(coef(opt)))




### rgenoud examples (from ?genoud)
library(rgenoud)
#maximize a univariate normal mixture which looks like a claw
claw <- function(xx) {
  x <- xx[1]
  y <- (0.46*(dnorm(x,-1.0,2.0/3.0) + dnorm(x,1.0,2.0/3.0)) +
  (1.0/300.0)*(dnorm(x,-0.5,.01) + dnorm(x,-1.0,.01) + dnorm(x,-1.5,.01)) +
  (7.0/300.0)*(dnorm(x,0.5,.07) + dnorm(x,1.0,.07) + dnorm(x,1.5,.07))) 
  return(y)
}
steps <- c()
clawsave <- function(x){
  steps <<- c(steps,x)
  claw(x)
}


lo=-5; hi=5
curve(sapply(x, claw),lo,hi,1e3)

steps<-c()
claw1   <- genoud(clawsave, nvars=1,pop.size=300,max=TRUE)
points(claw1$par, claw1$value, pch = 19, col='green',cex=3)
# show each of the points it tried
points(steps, sapply(steps,claw), pch=19, cex=.1, col='green', alpha=.5)
# compare to optimize
claw2 <- optimize(claw,c(lo,hi),maximum=T)
points(claw2$maximum, claw2$objective, pch = 19, col='red')










