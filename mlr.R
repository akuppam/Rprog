# Machine Learning with R.PDF
#############################

library(RWeka)
setwd("~/Pythprog/Rprog")
usedcars <- read.csv("usedcars.csv", header = TRUE)
summary(usedcars)
str(usedcars)

table(usedcars$year)
table(usedcars$model)

#C:\Users\akuppam\Documents\MAG_LM_TruckTour\StreetLight\Outputs
setwd("~/MAG_LM_TruckTour/StreetLight/Outputs")
slclean <- read.csv("streetlight_clean.csv", header = TRUE)
summary(slclean)
str(slclean)
# takes about 8:50 to execute the above block of code (same as Python with 1000 chunksize)

table(slclean$Vehicle_Type)               # freq tabs
prop.table(table(slclean$Vehicle_Type))   # percentage distributions

#plot(slclean$Time, slclean$Vehicle_Type)

library(gmodels)
CrossTable(x = slclean$Vehicle_Type, y = slclean$Point_Flag)

#C:\Users\akuppam\Documents\MAG_LM_TruckTour\StreetLight\Outputs
setwd("~/MAG_LM_TruckTour/StreetLight/Outputs")
slstops <- read.csv("streetlight_stops.csv", header = TRUE)
summary(slstops)
str(slstops)

###############
# Chapter 3 - kNN
###############
setwd("~/Pythprog/Rprog")
wbcd <- read.csv("wisc_bc_data.csv")
summary(wbcd)
str(wbcd)

wbcd <- wbcd[-1]
str(wbcd)

table(wbcd$diagnosis)
round(prop.table(table(wbcd$diagnosis))*100, digits=1)

# normalizing all columsn of data starting from 2nd column and ending at 31st column
wbcd_n <- as.data.frame(Normalize(wbcd[2:31]))
summary(wbcd_n)
summary(wbcd_n$area_mean)
str(wbcd_n)

# create train and test data
wbcd_train <- wbcd_n[1:469,]    # the first 469 records will be used for training or estimating parameters
wbcd_test <- wbcd_n[470:569,]    # the first 469 records will be used for training or estimating parameters

str(wbcd_train)
str(wbcd_test)

# adding back the 1st variable that was dropped earlier (column 1 - diagnosis)
wbcd_train_labels <- wbcd[1:469,1]
wbcd_test_labels <- wbcd[470:569,1]
str(wbcd_train_labels)
str(wbcd_test_labels)
str(wbcd)

# knn() is available from 'class' library
library(class)
# p <- knn(train, test, class, k)
# p - new vector returned
# train - training dataset
# test - test dataset
# class - the key vector or variable that is sort of dependent variable here from the training dataset
# k - number of nearest neighbors to be specified by the user

wbcd_pred <- knn(train=wbcd_train, test=wbcd_test, cl=wbcd_train_labels, k=3)
summary(wbcd_pred)
str(wbcd_pred)
View(wbcd_pred)
View(wbcd)

wbcd_test_pred <- knn(train=wbcd_train, test=wbcd_test, cl=wbcd_train_labels, k=21)
View(wbcd_test_pred)

CrossTable(x=wbcd_test_labels, y=wbcd_test_pred, prop.chisq=FALSE)
# Based on the CrossTable, a total of 1+4 = 5 cases were predicted wrong, that is, 5 out of 100;
# so the prediction success rate is 95% using k=21 (fyi, using k=3, it was 94% success rate)
plot(wbcd_test_pred, wbcd_test_labels)

library(ggplot2)
scatter.smooth(wbcd_test_pred, wbcd_test_labels)

# also, can use scale() as an alternaive to normalize() to transform the values to "z-score standardizations"
# z-score does not compress the values to the center like normalization does
# also, test out different k-values to see the differences in prediction success
# also, different sets (and/or number) of test records can be used

#########################
# Chapter 4 - Naive Bayes
#########################

# naiveBayes() uses Bayesian technique to model, test and predict results
# in the example mentioned in the book, it models spam results from a set of previous texts/emails

setwd("~/Pythprog/Rprog")
sms_raw <- read.csv("sms_spam.csv", stringsAsFactors = FALSE)
str(sms_raw)
table(sms_raw$type)

# load 'text mining' library
# corpus is a collection of text documents
library(tm)
sms_corpus <- Corpus(VectorSource(sms_raw$text))
print(sms_corpus)

# convert to lowercase
# remove any numbers
# remove stop words (e.g., to, and, but, or)
# remove punctuation
# remove whitespace

corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)
print(corpus_clean)

inspect(sms_corpus[1:5])
inspect(corpus_clean[1:5])

#library(wordcloud)
#wordcloud(corpus_clean, min_freq=40, random.order=FALSE)

sms_dtm <- DocumentTermMatrix(corpus_clean)

sms_raw_train <- sms_raw[1:4000,]
sms_raw_test <- sms_raw[4001:5574,]
sms_raw_test$type

sms_dtm_train <- sms_dtm[1:4000,] 
sms_dtm_test <- sms_dtm[4001:5574,]

sms_corpus_train <- corpus_clean[1:4000]
sms_corpus_test <- corpus_clean[4001:5574]

sms_dict <- findFreqTerms(sms_dtm_train,5)
# sms_dict <- Dictionary(findFreqTerms(sms_dtm_train,5))
# Dictionary() is NO longer available in R
sms_dict

sms_train <- DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
sms_test <- DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))
sms_test

convert_counts <- function(x){
  x <- ifelse(x>0,1,0)
  x <- factor(x, levels = c(0,1), labels = c("No", "Yes"))
  return(x)}

sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test <- apply(sms_test, MARGIN = 2, convert_counts)
sms_test

library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)
sms_classifier

sms_test_pred <- predict(sms_classifier, sms_test)
sms_test_pred

library(gmodels)
CrossTable(sms_test_pred, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

#################################
# Error in CrossTable(sms_test_pred, sms_raw_test$type, prop.chisq = FALSE,  : 
# x and y must have the same length
#################################

sms_classifier2 <- naiveBayes(sms_train, sms_raw_train$type, laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

#################################
# Need to look into this example - all over again - unable to run it
# Error in CrossTable(sms_test_pred2, sms_raw_test$type, prop.chisq = FALSE,  : 
# x and y must have the same length
# If it works, test sms_classifier with another sample of data (i.e., sms_test)  
#################################

#########################
# Chapter 5 - Decision Trees
#########################


# apply the C5.0 algorithm to VA model 

setwd('~/Pythprog/Rprog')
credit <- read.csv("credit.csv", header = TRUE)
va <- read.csv("va.csv", header = TRUE)

str(credit)
str(va)
View(credit)
View(va)
summary(va)

table(credit$checking_balance)
table(credit$savings_balance)
table(va$HHIncome)
table(va$HHPersons)
table(va$NumOfWorker)

table(credit$default)
table(va$VehAvailable)

# sort the datafiles randomly
set.seed(12345)
credit_rand <- credit[order(runif(1000)),]
va_rand <- va[order(runif(27206)),]
View(credit_rand)
View(va_rand)

credit_train <- credit_rand[1:900,]
credit_test <- credit_rand[901:1000,]

va_train <- va_rand[1:21000,]
va_test <- va_rand[21001:27206,]

prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

prop.table(table(va_train$VehAvailable))
prop.table(table(va_test$VehAvailable))

View(credit_train)
View(va_train)
str(credit_train)
str(va_train)

library(C50)
credit_model <- C5.0(credit_train[,-21], as.factor(credit_train$default))
va_model <- C5.0(va_train[,-5], as.factor(va_train$VehAvailable))

as.factor(credit_train$default)
as.factor(va_train$VehAvailable)

credit_model <- C5.0(credit_train[,-21], as.factor(credit_train$default), trials = 10)
va_model <- C5.0(va_train[,-5], as.factor(va_train$VehAvailable), trials = 10)

credit_model
va_model

summary(credit_model)
summary(va_model)

credit_pred <- predict(credit_model, credit_test)
va_pred <- predict(va_model, va_test)

library(gmodels)
CrossTable(credit_test$default, credit_pred, dnn = c('actual', 'predicted'))
CrossTable(va_test$VehAvailable, va_pred, dnn = c('actual', 'predicted'))
# ---------------------------------------------------------------------------------------------------
# THIS MODEL RESULTED IN PREDICTING VA BY 74.46% (4601 OUT OF 6206) - SUM OF DIAGONALS IN CROSS-TABLE
# ALWAYS REMOVE UNWANTED VARIABLES ESPECIALLY THE ONES WITH CHARACTERS AND STRONGS THAT DON'T HAVE ANY
# IMPACT ON THE DEPENDENT VARIABLE (VA)
# ---------------------------------------------------------------------------------------------------

#########################
# Chapter 6 - Regression Models
#########################

library(ggplot2)
pairs(va)
ggplot(va, aes(x = va$VehAvailable, y = va$HHIncome, colour = va$NumOfWorker)) + geom_point()

library(psych)
pairs.panels(va)

summary(va)

# sort the datafiles randomly
set.seed(1000)
va_rand <- va[order(runif(27206)),]
View(va_rand)

va_train <- va_rand[1:22000,]
va_test <- va_rand[22001:27206,]
View(va_test)

#----------------------------
# lm(), or linear regression, or OLS
#----------------------------

va_model <- lm(va_train$VehAvailable ~ va_train$HHIncome + va_train$HHPersons + va_train$NumOfWorker)
summary(va_model)

va_predict <- predict(va_model, va_test)
summary(va_predict)
summary(va_rand$VehAvailable)

va_predict <- predict(va_model, va_train)
summary(va_predict)
summary(va_rand$VehAvailable)

scatter.hist(va_test, va_predict)
scatter.smooth(va_test, va_predict)

scatter.hist(va_train, va_predict)
scatter.smooth(va_train, va_predict)

scatter.hist(va_rand, va_predict)
scatter.smooth(va_rand, va_predict)

scatter.hist(va, va_predict)
scatter.smooth(va, va_predict)

#----------------------------
# rpart(), or recursive partitioning
#----------------------------
library(rpart)

va_model1 <- rpart(va_train$VehAvailable ~ va_train$HHPersons + va_train$HHIncome + va_train$NumOfWorker)
summary(va_model1)
va_model1

library(rpart.plot)
rpart.plot(va_model1, digits = 3)
rpart.plot(va_model1, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)

va_predict1 <- predict(va_model1, va_test)
summary(va_predict1)
summary(va_test$VehAvailable)

cor(va_predict1, va_test$VehAvailable)
cor(va_predict1, va_train$VehAvailable)

#########################
# Chapter 7 - Neural Networks
#########################

summary(va)
library(neuralnet)
va_model2 <- neuralnet(va$VehAvailable ~ va$HHPersons + va$HHIncome + va$NumOfWorker, data=va, hidden = 1)
va_model2
summary(va_model2)
plot(va_model2)

va_model2 <- neuralnet(va$VehAvailable ~ va$HHIncome, data=va, hidden = 1)
va_model2
summary(va_model2)
plot(va_model2)

summary(credit)
credit_model2 <- neuralnet(credit$default ~ credit$amount, data=credit, hidden = 1)
credit_model2
summary(credit_model2)
plot(credit_model2)

#----------------------------
# SVM or support vector machines
#----------------------------

library(kernlab)
va_model3 <- ksvm(va$VehAvailable ~ va$HHIncome, data = va_train, kernel = "vanilladot")
va_model3
summary(va_model3)

va_predict3 <- predict(va_model3, va_test)
va_predict3
summary(va_predict3)
table(va_predict3, va_test$VehAvailable)
table(va_predict3, va_train$VehAvailable)
table(va_predict3, va$VehAvailable)
summary(va$VehAvailable)
scatter.smooth(va_predict3, va$VehAvailable)
scatter.hist(va_predict3, va$VehAvailable)

agreement <- va_predict3 == va$VehAvailable
table(agreement)

# -------------------

library(kernlab)
credit_model3 <- ksvm(credit$default ~ credit$amount, data = credit_train, kernel = "vanilladot")
credit_model3
summary(credit_model3)

credit_predict3 <- predict(credit_model3, credit_test)
credit_predict3
summary(credit_predict3)
table(credit_predict3, credit_test$default)
table(credit_predict3, credit_train$default)
table(credit_predict3, credit$default)
summary(credit$default)
scatter.smooth(credit_predict3, credit$default)
scatter.hist(credit_predict3, credit$default)

agreement <- credit_predict3 == credit$default
table(agreement)

#########################
# Chapter 8 - Market basket analysis
#########################

# --------------------
# apriori algorithm
# --------------------

setwd('~/Pythprog/Rprog')
groceries <- read.csv("groceries.csv", sep=',', header = FALSE)
View(groceries)

groceries1 <- read.csv("groceries.csv", sep=',')
View(groceries1)

library(arules)
groceries2 <- read.transactions("groceries.csv", sep=',')
summary(groceries2)

inspect(groceries2[1:5])
itemFrequency(groceries2[,1:5])
itemFrequencyPlot(groceries2, support= 0.1)
itemFrequencyPlot(groceries2, topN = 20)
image(groceries2[1:10])
image((sample(groceries2,100)))

groceryrules <- apriori(groceries2, parameter = list(support = 0.006, confidence = 0.25, minlen = 2))
groceryrules
summary(groceryrules)
inspect(groceryrules[1:5])
inspect(sort(groceryrules, by="lift")[1:10])
# "lift" indicates by how much more likely are people to buy LHS and RHS items
# partial output shown BELOW
#
#
#> inspect(sort(groceryrules, by="lift")[1:10])
#lhs                                             rhs                  support        confidence  
#3   {herbs}                                      => {root vegetables}    0.007015760041 0.4312500000
#57  {berries}                                    => {whipped/sour cream} 0.009049313676 0.2721712538
#450 {other vegetables,tropical fruit,whole milk} => {root vegetables}    0.007015760041 0.4107142857
#lift       
#3   3.956477379
#57  3.796885505
#450 3.768073694
# ------------------------------------------------

#########################
# Chapter 9 - Cluster Analysis with k-means
#########################


setwd('~/Pythprog/Rprog')
teens <- read.csv("snsdata.csv")
summary(teens)
str(teens)

table(teens$gender)
table(teens$gender, useNA = "ifany")

teens$female <- ifelse(teens$gender =="F" & !is.na(teens$gender), 1, 0)
table(teens$female, useNA = "ifany")

teens$no_gender <- ifelse(is.na(teens$gender), 1, 0)
table(teens$no_gender, useNA = "ifany")

library(stats)
interests <- teens[5:40]  # variables that have the likert scale like values to be used for the cluster analysis
str(interests)

interests_z <- as.data.frame(lapply(interests, scale))  # applying z-scores standardization
str(interests_z)

teen_clusters <- kmeans(interests_z, 5)   # create 5 clusters
summary(teen_clusters)
str(teen_clusters)
teen_clusters$size
teen_clusters$centers

# the following produces outputs of scores 
# similar scores across the likert scale like vars will help combine/name clusters

teens$cluster <- teen_clusters$cluster     
summary(teens$cluster)

# look at summary of other vars by cluster

aggregate(data = teens, age ~ cluster, mean)
aggregate(data = teens, female ~ cluster, mean)
aggregate(data = teens, friends ~ cluster, mean)

# useful graphics for clusters
library(cluster)
library(HSAUR)
library(fpc)

plotcluster(teens, teen_clusters$cluster)
clusplot(teens, teen_clusters$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
with(teens, pairs(teens, col=c(1:3)[teen_clusters$cluster]))


