words <- 
  
setwd("~/Pythprog/Rprog")

library(twitteR)
library(tm)
library(wordcloud)

############################################
# DEFAULT AND REQUIRED cleaN.text FUNCTION - JUST COPY/PASTE
############################################
clean.text = function(x)
{
  # tolower
  x = tolower(x)
  # remove rt
  x = gsub("rt", "", x)
  # remove at
  x = gsub("@\\w+", "", x)
  # remove punctuation
  x = gsub("[[:punct:]]", "", x)
  # remove numbers
  x = gsub("[[:digit:]]", "", x)
  # remove links http
  x = gsub("http\\w+", "", x)
  # remove tabs
  x = gsub("[ |\t]{2,}", "", x)
  # remove blank spaces at the beginning
  x = gsub("^ ", "", x)
  # remove blank spaces at the end
  x = gsub(" $", "", x)
  return(x)
}
######################################################
# MAKE ANY CHANGES TO THE BELOW CODE, FILE NAME, LABELS, COLORS, ETC
######################################################
# Import csv
txt1.csv <- read.csv("C:/Users/akuppam/Documents/Pythprog/Rprog/guys.csv")
txt1.csv$text <- as.factor(txt1.csv$text)
hm1_clean = clean.text(txt1.csv$text)

txt2.csv <- read.csv("C:/Users/akuppam/Documents/Pythprog/Rprog/gals.csv")
txt2.csv$text <- as.factor(txt2.csv$text)
hm2_clean = clean.text(txt2.csv$text)

hm1 = paste(hm1_clean, collapse=" ")
hm2 = paste(hm2_clean, collapse=" ")

all = c(hm1, hm2)

# remove stop-words
all = removeWords(all, c(stopwords("english"), "the", "and", "or"))

# create corpus
corpus = Corpus(VectorSource(all))

# create term-document matrix
tdm = TermDocumentMatrix(corpus)

# convert as matrix
tdm = as.matrix(tdm)

# add column names
#colnames(tdm) = c("A", "B")

# comparison cloud
comparison.cloud(tdm, random.order=FALSE, 
                 colors = c("#00B2FF", "red"),
                 title.size = 1.5, max.words=1000)

# commonality cloud
commonality.cloud(tdm, random.order=FALSE,
                  colors = brewer.pal(8, "Dark2"),
                  title.size = 1.5)


######################################################
