#install and load the rrequired packages
install.packages("rJava")
library("rJava")
install.packages(c("NLP", "openNLP", "RWeka", "qdap"))
library(NLP)
library("tm")
library("SnowballC")
library("RColorBrewer")
library("wordcloud")
#choose the input file
text1 <- readLines(file.choose())
#load it into R
docs <- Corpus(VectorSource(text1))
inspect(docs)
#data cleaning
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
#create a document term matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
dhead <- head(d, 10)
set.seed(1234)
#word cloud in color
wordcloud(words = d$word, freq = d$freq, min.freq = 100,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
mydata <- na.omit(dhead) # listwise deletion of missing
fit <- kmeans(mydata, 5)
d <- dist(mydata, method = "euclidean")
fit <- hclust(d, method="ward") 
plot(fit)
#choose the input file
text2 <- readLines(file.choose())
#load it into R
docs1 <- Corpus(VectorSource(text2))
inspect(docs1)
#data cleaning
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs1 <- tm_map(docs1, toSpace, "/")
docs1 <- tm_map(docs1, toSpace, "@")
docs1 <- tm_map(docs1, toSpace, "\\|")
docs1 <- tm_map(docs1, content_transformer(tolower))
docs1 <- tm_map(docs1, removeNumbers)
docs1 <- tm_map(docs1, removeWords, stopwords("english"))
docs1 <- tm_map(docs1, removeWords, c("blabla1", "blabla2")) 
docs1 <- tm_map(docs1, removePunctuation)
docs1 <- tm_map(docs1, stripWhitespace)
#create a document term matrix
dtm1 <- TermDocumentMatrix(docs1)
m1 <- as.matrix(dtm1)
v1 <- sort(rowSums(m1),decreasing=TRUE)
d1 <- data.frame(word = names(v1),freq=v1)
dhead1 <- head(d1, 10)
set.seed(1234)
#word cloud in color
wordcloud(words = d1$word, freq = d1$freq, min.freq = 100,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
mydata1 <- na.omit(dhead1) # listwise deletion of missing
fit <- kmeans(mydata1, 5)
d1 <- dist(mydata1, method = "euclidean")
fit <- hclust(d1, method="ward") 
plot(fit)
#to find the common in two literatures
intersect(dtm,dtm1)
