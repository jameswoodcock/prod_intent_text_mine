#Clear workspace
rm(list=ls())

#Load libraries
library(tm)
library(ggplot2)
library(SnowballC)   

system = "all"

#set some paths
figPath <- paste("./figures/",system,"/",sep="")
cname <- file.path(".",paste("texts_",system,sep=""))		#Set document path

##Load and prepare documents
docs <- Corpus(DirSource(cname))		#Load Corpus
doc_labels <- summary(docs)				#Get  labels of documents

docs <- tm_map(docs, removePunctuation)	#Remove punctuation
docs <- tm_map(docs, removeNumbers) 	#Remove numbers
docs <- tm_map(docs, tolower) 			#Force lower case
docs <- tm_map(docs, stripWhitespace) 	#Remove white space
docs <- tm_map(docs, stemDocument)		#Stem documents

##Make a copy of docs before we start removing stop words
docs_raw <- docs
docs_raw <- tm_map(docs_raw, PlainTextDocument)	#Make plain text

##Calculate raw freqeuncies
dtm_raw <- DocumentTermMatrix(docs_raw)	#Make document term matrix
freq_raw_ord <- sort(colSums(as.matrix(dtm_raw)), decreasing=TRUE)	#Get freqeuncies and order  

Nwords = 75		#how many words to plot
wf_raw = data.frame(word=names(freq_raw_ord[1:Nwords ]), freq = freq_raw_ord[1:Nwords ])	#Make data frame for plotting
wf_raw$word = factor(wf_raw$word,levels=wf_raw[order(wf_raw$freq),"word"])		#Set order as a factor, so that we can plot in ascending order

p <- ggplot(wf_raw,aes(x = word,y = freq)) + geom_bar(stat="identity") 
p <- p + theme_bw()
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1)) + ggtitle(system) + ylab("Count") + xlab("")
pdf(paste(figPath,"word_frequencies_raw.pdf",sep=""),width = 12, height = 8)
p
dev.off()

##Remove some common stop words from our Corpusmy
myStopwords <- setdiff(stopwords("english"),c("during","between","into","before","after","above","below","up","on","off","over","under","again","down","in","out","further","here","there"))
##Add my own stop words
myStopwords <- c(myStopwords,c("sound","sounds","bit","think","nothing","also"))

docs <- tm_map(docs, removeWords, myStopwords)

docs <- tm_map(docs, PlainTextDocument)	#Make plain text

##Calculate freqeuncies
dtm <- DocumentTermMatrix(docs)	#Make document term matrix
freq_ord <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)	#Get freqeuncies and order  

Nwords = 75		#how many words to plot
wf = data.frame(word=names(freq_ord[1:Nwords ]), freq = freq_ord[1:Nwords ])	#Make data frame for plotting
wf$word = factor(wf$word,levels=wf[order(wf$freq),"word"])		#Set order as a factor, so that we can plot in ascending order

p <- ggplot(wf,aes(x = word,y = freq)) + geom_bar(stat="identity") 
p <- p + theme_bw()
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1)) + ggtitle(system) + ylab("Count") + xlab("")
pdf(paste(figPath,"word_frequencies.pdf",sep=""),width = 12, height = 8)
p
dev.off()

#Find associations for top 20 terms
Nterms = 15
associations <- list()
topTerms <- names(head(freq_ord,Nterms))
for (i in 1:Nterms) {
associations[[i]] <- findAssocs(dtm,topTerms[i],0.3)
write.table(associations[[i]],file=paste(figPath,"associations_",topTerms[i],'.txt',sep=""),sep=" ")
}
print(associations)