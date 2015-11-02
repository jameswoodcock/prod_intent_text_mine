#Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")   
# install.packages(Needed, dependencies=TRUE)   
   
# install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source") 

cname = file.path(".","texts")
##Load text mining libray and build corpus
library(tm)
docs <- Corpus(DirSource(cname))
doc_labels <- summary(docs)

##Remove punctuation, numbers, and convert to lower case
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers) 
docs <- tm_map(docs, tolower) 
docs <- tm_map(docs, removeWords, c(stopwords("english"),"bit","sounds","sound","downmix"))
docs <- tm_map(docs, stripWhitespace) 

##Stem docs
library(SnowballC)   
docs <- tm_map(docs, stemDocument)

docs <- tm_map(docs, PlainTextDocument)

dtm <- DocumentTermMatrix(docs)
rownames(dtm) <- rownames(doc_labels) 
dtm 
tdm <- TermDocumentMatrix(docs) 
colnames(tdm) <- rownames(doc_labels)   
tdm 

#  Start by removing sparse terms:   
dtms <- removeSparseTerms(dtm, 0.98) # This makes a matrix that is 10% empty space, maximum.   
inspect(dtms) 

freq <- colSums(as.matrix(dtm))   
ord <- order(freq)

freq[head(ord)]
freq[tail(ord)]

d <- dist(t(dtms), method="euclidian")   
fit <- hclust(d=d, method="ward")   
fit
cut = cutree(fit,k=5)
plot(fit,hang=-1,cex=1)
#rect.hclust(fit,k=5)

#Table of clustered terms
cutMat <- as.matrix(cutree(fit,k=8))
clusterTable <- split(row.names(cutMat),cutMat)

##kmeans clustering
library(cluster)
library(fpc)
Nclust = 4   
d <- dist(t(dtms), method="euclidian")   
kfit <- kmeans(d, Nclust)   
#clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)  