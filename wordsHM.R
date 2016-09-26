words <- 
  
setwd("~/Pythprog/Rprog")

library(tm)
library(wordcloud)

lords <- Corpus (DirSource("hm/"))

#lords <- Corpus (DirSource("temp/"))

# lords <- read.csv("adb10-graphics.csv", header = TRUE)

#To see what's in that corpus, type the command

inspect(lords)

#This should print out contents on the main screen. 
#Next, we need to clean it up. 
#Execute the following in the command line, one line at a time:
  
lords <- tm_map(lords, stripWhitespace)

lords <- tm_map(lords, tolower)

lords <- tm_map(lords, removeWords, stopwords("english"))

lords <- tm_map(lords, stemDocument)

lords <- tm_map(lords, PlainTextDocument)

wordcloud(lords, scale=c(5,0.5), max.words=500, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
wordcloud(lords, scale=c(2,0.5), max.words=500, random.order=FALSE, rot.per=0.25, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
wordcloud(lords, scale=c(2,0.25), max.words=500, random.order=FALSE, rot.per=0.15, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
wordcloud(lords, scale=c(2,0.5), max.words=500, random.order=FALSE, rot.per=0.15, use.r.layout=TRUE, colors=brewer.pal(8, "Dark2"))
wordcloud(lords, scale=c(3,0.25), max.words=500, random.order=FALSE, rot.per=0.10, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
wordcloud(lords, scale=c(3,0.25), max.words=1500, random.order=FALSE, rot.per=0.10, use.r.layout=FALSE, colors=brewer.pal(20, "Dark2"))
wordcloud(lords, scale=c(2.5,0.25), random.order=FALSE, rot.per=0.10, use.r.layout=FALSE, colors=brewer.pal(20, "Dark2"))
wordcloud(lords, scale=c(1,0.05), random.order=FALSE, rot.per=0.01, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
wordcloud(lords, scale=c(1,0.05), random.order=FALSE, rot.per=0.01, use.r.layout=FALSE)
wordcloud(lords, random.order=FALSE, rot.per=0.9, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
