# ***********************
# support vector machines (page 359; section 9.6) 
# ***********************
y = as.factor(y)
y

a = as.factor(a)
a

x = as.factor(x)
x
View(x)

dat = data.frame(x=x, y = as.factor(y))
View(dat)
summary(dat)
library(e1071)
svmfit = svm(y ~., data=dat, kernel = "linear", cost = 10, scale = TRUE)
svmfit
plot(svmfit,dat)


