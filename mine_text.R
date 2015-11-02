#Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")   
# install.packages(Needed, dependencies=TRUE)   
   
# install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source") 
#source("http://bioconductor.org/biocLite.R")
#    biocLite("Rgraphviz")


rm(list=ls())
library(SnowballC)


figPath <- "./figures/"

cname = file.path(".","texts")
##Load text mining libray and build corpus
library(tm)
docs <- Corpus(DirSource(cname))
doc_labels <- summary(docs)

##Remove punctuation, numbers, and convert to lower case
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers) 
docs <- tm_map(docs, tolower) 
docs <- tm_map(docs, stripWhitespace) 

#Calculate raw freqeuncies
docs_raw <- tm_map(docs, PlainTextDocument)
dtm_raw <- DocumentTermMatrix(docs_raw)
freq_raw <- colSums(as.matrix(dtm_raw))   
ord_raw <- order(freq_raw)
freq_raw_ord <- sort(colSums(as.matrix(dtm_raw)), decreasing=TRUE)  

Nwords = 75		#how many words to plot
wf = data.frame(word=names(freq_raw_ord[1:Nwords ]), freq = freq_raw_ord[1:Nwords ])	#Make data frame for plotting
wf$word = factor(wf$word,levels=wf[order(wf$freq),"word"])

library(ggplot2)
p <- ggplot(wf,aes(x = word,y = freq)) + geom_bar(stat="identity") 
p <- p + theme_bw()
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
pdf(paste(figPath,"word_frequencies_raw.pdf",sep=""),width = 12, height = 8)
p
dev.off()

#stopwords
myStopwords <- setdiff(stopwords("english"),c("during","between","into","before","after","above","below","up","on","off","over","under","again","down","in","out","further","here","there"))
myStopwords <- c(myStopwords,"also","nothing","bit","make","slightly","think","good")
docs <- tm_map(docs, removeWords, myStopwords)

##Stem docs
library(SnowballC)   
docs <- tm_map(docs, stemDocument)

#Calculate new freqeuncies
docs <- tm_map(docs, PlainTextDocument)
dtm <- DocumentTermMatrix(docs)
freq <- colSums(as.matrix(dtm))   
ord <- order(freq)
freq_ord <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)  

Nwords = 75		#how many words to plot
wf = data.frame(word=names(freq_ord), freq = freq_ord)	#Make data frame for plotting
wf$word = factor(wf$word,levels=wf[order(wf$freq),"word"])

library(ggplot2)
p <- ggplot(wf[1:Nwords, ],aes(x = word[1:Nwords],y = freq[1:Nwords])) + geom_bar(stat="identity") 
p <- p + theme_bw()
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
pdf(paste(figPath,"word_frequencies.pdf",sep=""),width = 12, height = 8)
p
dev.off()

# Remove stop words

#docs <- tm_map(docs, removeWords, c(stopwords("english"),"nothing","bit","sounds","sound","downmix"))

dtm <- DocumentTermMatrix(docs)
rownames(dtm) <- rownames(doc_labels) 
dtm 
tdm <- TermDocumentMatrix(docs) 
colnames(tdm) <- rownames(doc_labels)   
tdm 

# Remove words that only occur once
library(graph)
library(Rgraphviz)
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 5)

pdf(paste(figPath,"assoc_map.pdf",sep=""),width = 32, height = 36)
plot(tdm,term = names(term.freq),corThreshold = 0.1,weighting=T)
dev.off()

#  Start by removing sparse terms:   
#dtms <- removeSparseTerms(dtm, 0.95) # This makes a matrix that is 10% empty space, maximum.   
#inspect(dtms) 

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

pdf(paste(figPath,"clustergram.pdf",sep=""),width = 32, height = 36)
heatmap(as.matrix(dtms),hclustfun=function(d) hclust(d,method='ward.D2'),margins=c(20,20),scale='none',col=grey(seq(0.9
,0,-0.01)),main=paste("files x words"))
dev.off()

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