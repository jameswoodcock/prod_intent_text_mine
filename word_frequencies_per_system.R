#Clear workspace
rm(list=ls())

#Load libraries
library(tm)
library(ggplot2)
library(SnowballC)   

set.seed(12345)
system = "all"

#set some paths
figPath <- paste("./figures/",system,"/",sep="")
cname <- file.path(".",paste("texts_",system,sep=""))		#Set document path

##Load and prepare documents
docs <- Corpus(DirSource(cname))		#Load Corpus
docs_orig <- docs 						#Copy of original corpus
doc_labels <- summary(docs)				#Get  labels of documents

docs <- tm_map(docs, removePunctuation)	#Remove punctuation
docs <- tm_map(docs, removeNumbers) 	#Remove numbers
docs <- tm_map(docs, tolower) 			#Force lower case
docs <- tm_map(docs, stripWhitespace) 	#Remove white space



##Make a copy of docs before we start removing stop words
docs_raw <- docs
docs_raw <- tm_map(docs_raw, PlainTextDocument)	#Make plain text

##Calculate raw freqeuncies
dtm_raw <- DocumentTermMatrix(docs_raw)	#Make document term matrix

pdf(paste(figPath,"terms_in_doc_matrix.pdf",sep=""),width = 12, height = 8)
hist(apply(dtm_raw, 1, sum), xlab="Number of Terms in Term-Document Matrix", main="",breaks=50)
dev.off()


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
#myStopwords <- c(myStopwords,c("sound","sounds","bit","think","nothing","also","downmix","good","better","mix",
#	"slightly","seems","there","needs","little","get","like","downmix","mix"))
myStopwords <- c(myStopwords,c("doesnt","sound","sounds","bit","think","also","downmix","good","better",
                               "slightly","seems","there","needs","little","get","like","feels","mix","clips","clip","lack",
                               "increase","decrease","lower","raise","slight","better","thinner","nice","perfect",
                               "cleaner","very","low","sudden","slight","too","excllent","reduce"))
myStopwords <- c(myStopwords,c("actors","hubub","man","child","Children","vo","dialogue",
                               "loudhailer","lady","kid","Voice","crowd","dog","bark",
                               "violins","music","chord","narrator","narrators","woodpecker",
                               "megaphone","dad","humming","chartacters","speach",
                               "monster","son","girls","tom","shouting","boy","ducks","synth",
                               "dinosaur","dialogues","barking","footsteps","male","vocal","girl",
                               "speach","megafono","thump","goose","character","background"))#remove objects
docs <- tm_map(docs, removeWords, myStopwords)
#Remove words that only occur once
docs <- tm_map(docs, removeWords, names(subset(freq_raw_ord,freq_raw_ord==1)))

docs <- tm_map(docs, stemDocument)		#Stem documents
#Make plain text
docs <- tm_map(docs, PlainTextDocument)	

##Calculate freqeuncies
dtm <- DocumentTermMatrix(docs)	#Make document term matrix
tdm <- TermDocumentMatrix(docs)	#Make term document matrix
rownames(dtm) <- rownames(doc_labels) 	#Original document names
colnames(tdm) <- rownames(doc_labels)   
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
corThreshold = 0.2	#Correlation threshold
associations <- list()
topTerms <- names(head(freq_ord,Nterms))
for (i in 1:Nterms) {
associations <- findAssocs(dtm,topTerms[i],corThreshold)
#for (j in 1:length(associations[[1]])){
#write(names(associations[[1]][j]),file=paste(figPath,i,topTerms[i],'.txt'),append=TRUE)
#}
#write.table(associations[[i]],file=paste(figPath,"associations_",topTerms[i],'.txt',sep=""),sep=" ")
}
print(associations)

write(topTerms,file=paste(figPath,system,"_top_terms.txt",sep=""))


###################################
####CLUSTERING#####################
###################################


#accumulator for cost results
cost_df <- data.frame()

for(i in 1:100){
  #Run kmeans for each level of i, allowing up to 100 iterations for convergence
  kmeans<- kmeans(x=dtm, centers=i, iter.max=100)
  
  #Combine cluster number and cost together, write to df
  cost_df<- rbind(cost_df, cbind(i, kmeans$tot.withinss))
  
}
names(cost_df) <- c("cluster", "cost")

#Calculate lm's for emphasis
fit1 <- lm(cost_df$cost[1:19] ~ cost_df$cluster[1:19])
fit2 <- lm(cost_df$cost[20:59] ~ cost_df$cluster[20:59])
fit3 <- lm(cost_df$cost[60:100] ~ cost_df$cluster[60:100])

cost_df$fitted <- ifelse(cost_df$cluster <20, (fit1[[1]][1] + fit1[[1]][2]*cost_df$cluster), 
                         ifelse(cost_df$cluster <60, (fit2[[1]][1] + fit2[[1]][2]*cost_df$cluster),
                                (fit3[[1]][1] + fit3[[1]][2]*cost_df$cluster)))

#Cost plot
pdf(paste(figPath,"scree_plot.pdf",sep=""),width = 12, height = 8)
ggplot(data=cost_df, aes(x=cluster, y=cost, group=1)) + 
  theme_bw() + 
  geom_line(colour = "darkgreen") +
  theme(text = element_text(size=20)) +
#  ggtitle("Reduction In Cost For Values of 'k'\n") +
  xlab("\nClusters") + 
  ylab("Within-Cluster Sum of Squares\n") +
  scale_x_continuous(breaks=seq(from=0, to=100, by= 10)) +
  geom_line(aes(y= fitted), linetype=2)
dev.off()

Nclusters_kmeans = 15
Nclusters_kmeans_sub = 10
kmeans <- kmeans(dtm,Nclusters_kmeans)

#Table of clustered terms
cutMat_kmeans <- as.matrix(kmeans$cluster)
clusterTable_kmeans <- split(row.names(cutMat_kmeans),cutMat_kmeans)

orderClusterSize <- order(sapply(clusterTable_kmeans, length),decreasing=T)

clusterTable_kmeans<-clusterTable_kmeans[orderClusterSize]

bigClusterList_kmeans <- list()
mainClusterTerm_kmeans <- list()
write("",file=paste(figPath,"clusters_kmeans.txt",sep=""),append=FALSE)	#Clear file
write("",file=paste(figPath,"clusters_kmeans_latex.txt",sep=""),append=FALSE)	#Clear file
for (writeCluster in 1:Nclusters_kmeans){
  listClusters <- list()
  write(paste("\n===================\n=====Cluster ",writeCluster,"=====\n===================\n",sep=""),file=paste(figPath,"clusters_kmeans.txt",sep=""),append=TRUE)
  write(paste("\\section*{Cluster ",writeCluster,"}\n",sep=""),file=paste(figPath,"clusters_kmeans_latex.txt",sep=""),append=TRUE)
  for (i in 1:length(clusterTable_kmeans [[writeCluster]])) {
    if (i != length(clusterTable_kmeans [[writeCluster]])){
    write(paste(clusterTable_kmeans [[writeCluster]][i],"    ",as.character(docs_orig[[clusterTable_kmeans [[writeCluster]][i]]])),file=paste(figPath,"clusters_kmeans.txt",sep=""),append=TRUE)
      write(paste("*",as.character(docs_orig[[clusterTable_kmeans [[writeCluster]][i]]]),"\\newline"),file=paste(figPath,"clusters_kmeans_latex.txt",sep=""),append=TRUE)
    listClusters[i] <- as.character(docs_orig[[clusterTable_kmeans [[writeCluster]][i]]])}
    else if (i == length(clusterTable_kmeans [[writeCluster]])){ 
      write(paste(clusterTable_kmeans [[writeCluster]][i],"    ",as.character(docs_orig[[clusterTable_kmeans [[writeCluster]][i]]])),file=paste(figPath,"clusters_kmeans.txt",sep=""),append=TRUE)
      write(paste("*",as.character(docs_orig[[clusterTable_kmeans [[writeCluster]][i]]]),"\\newline"),file=paste(figPath,"clusters_kmeans_latex.txt",sep=""),append=TRUE)
    listClusters[i] <- as.character(docs_orig[[clusterTable_kmeans [[writeCluster]][i]]])
    bigClusterList_kmeans[[writeCluster]] <- listClusters

    docsCluster <- Corpus(VectorSource(bigClusterList_kmeans[[writeCluster]]))
    docsCluster <- tm_map(docsCluster, removePunctuation)	#Remove punctuation
    docsCluster <- tm_map(docsCluster, removeNumbers) 	#Remove numbers
    docsCluster <- tm_map(docsCluster, tolower) 			#Force lower case
    docsCluster <- tm_map(docsCluster, stripWhitespace) 	#Remove white space
    
    docsCluster <- tm_map(docsCluster, removeWords, myStopwords)
    #Remove words that only occur once
    docsCluster <- tm_map(docsCluster, removeWords, names(subset(freq_raw_ord,freq_raw_ord==1)))
    #Make plain text
    docsCluster <- tm_map(docsCluster, stemDocument)		#Stem documents
    docsCluster <- tm_map(docsCluster, PlainTextDocument)	
    ##Calculate freqeuncies
    dtmCluster <- DocumentTermMatrix(docsCluster)	#Make document term matrix   
    
    freq_ordCluster <- sort(colSums(as.matrix(dtmCluster)), decreasing=TRUE)
    mainClusterTerm_kmeans[[writeCluster]] <- head(freq_ordCluster,3)

    inGroup <- which(kmeans$cluster==orderClusterSize[writeCluster])
    within <- dtm[inGroup,]
    out <- dtm[-inGroup,]
    words <- apply(within,2,mean) - apply(out,2,mean)
    labels <- order(words, decreasing=T)[1:3]
    write("\n",file=paste(figPath,"clusters_kmeans.txt",sep=""),append=TRUE)
    write.table(names(words)[labels],file=paste(figPath,"clusters_kmeans.txt",sep=""),append=TRUE,col.names = FALSE,quote=FALSE)
    write("\n",file=paste(figPath,"clusters_kmeans_latex.txt",sep=""),append=TRUE)
    write(paste("\\noindent Number of documents in cluster: ",i,"\\newline",sep=""),file=paste(figPath,"clusters_kmeans_latex.txt",sep=""),append=TRUE)
    write(paste("\\noindent Percentage of corpus: ",format(i/length(doc_labels)*100,digits = 2,nsmall=1),"\\newline",sep=""),file=paste(figPath,"clusters_kmeans_latex.txt",sep=""),append=TRUE)
    write("\n",file=paste(figPath,"clusters_kmeans_latex.txt",sep=""),append=TRUE)
    write("\\noindent\\textbf{Cluster labels:}\\newline",file=paste(figPath,"clusters_kmeans_latex.txt",sep=""),append=TRUE)
    write(paste("\\noindent",names(words)[labels][1]),file=paste(figPath,"clusters_kmeans_latex.txt",sep=""),append=TRUE)
    write("\\newline",file=paste(figPath,"clusters_kmeans_latex.txt",sep=""),append=TRUE)
    write(names(words)[labels][2],file=paste(figPath,"clusters_kmeans_latex.txt",sep=""),append=TRUE)
    write("\\newline",file=paste(figPath,"clusters_kmeans_latex.txt",sep=""),append=TRUE)
    write(names(words)[labels][3],file=paste(figPath,"clusters_kmeans_latex.txt",sep=""),append=TRUE)
    write("\\newline",file=paste(figPath,"clusters_kmeans_latex.txt",sep=""),append=TRUE)
    write("\n",file=paste(figPath,"clusters_kmeans_latex.txt",sep=""),append=TRUE)
   write(paste("\\includegraphics[width=0.6\\linewidth]{./figures/cluster",writeCluster,".pdf}\n",sep=""),file=paste(figPath,"clusters_kmeans_latex.txt",sep=""),append=TRUE)
   
    }
  }
}

#Try a subcluser
s<-summary(clusterTable_kmeans)
labels_sub<-clusterTable_kmeans[which.max(s[,1])]#Get  labels of documents
docsSubCluster_orig <- Corpus(VectorSource(bigClusterList_kmeans[[which.max(s[,1])]]))

docsSubCluster <- Corpus(VectorSource(bigClusterList_kmeans[[which.max(s[,1])]]))
docsSubCluster <- tm_map(docsSubCluster, removePunctuation)	#Remove punctuation
docsSubCluster <- tm_map(docsSubCluster, removeNumbers) 	#Remove numbers
docsSubCluster <- tm_map(docsSubCluster, tolower) 			#Force lower case
docsSubCluster <- tm_map(docsSubCluster, stripWhitespace) 	#Remove white space

docsSubCluster <- tm_map(docsSubCluster, removeWords, myStopwords)
#Remove words that only occur once
docsSubCluster <- tm_map(docsSubCluster, removeWords, names(subset(freq_raw_ord,freq_raw_ord==1)))
#Make plain text
docsSubCluster <- tm_map(docsSubCluster, stemDocument)		#Stem documents
docsSubCluster <- tm_map(docsSubCluster, PlainTextDocument)	
##Calculate freqeuncies
dtmSubCluster <- DocumentTermMatrix(docsSubCluster)	#Make document term matrix


rownames(dtmSubCluster)<-labels_sub[[1]]


kmeansSub <- kmeans(dtmSubCluster,Nclusters_kmeans_sub)

#Table of clustered terms
cutMat_kmeans_sub <- as.matrix(kmeansSub$cluster)
clusterTable_kmeans_sub <- split(row.names(cutMat_kmeans_sub),cutMat_kmeans_sub)

orderClusterSizeSub <- order(sapply(clusterTable_kmeans_sub, length),decreasing=T)
clusterTable_kmeans_sub<-clusterTable_kmeans_sub[orderClusterSizeSub]

bigClusterList_kmeans_sub <- list()
mainClusterTerm_kmeans_sub <- list()
write("",file=paste(figPath,"clusters_kmeans_sub.txt",sep=""),append=FALSE)	#Clear file
write("",file=paste(figPath,"clusters_kmeans_sub_latex.txt",sep=""),append=FALSE)	#Clear file
for (writeCluster in 1:Nclusters_kmeans_sub){
  listClusters_sub <- list()
  write(paste("\n===================\n=====Cluster ",writeCluster,"=====\n===================\n",sep=""),file=paste(figPath,"clusters_kmeans_sub.txt",sep=""),append=TRUE)
  write(paste("\\section*{Cluster ",writeCluster,"}\n",sep=""),file=paste(figPath,"clusters_kmeans_sub_latex.txt",sep=""),append=TRUE)
  for (i in 1:length(clusterTable_kmeans_sub [[writeCluster]])) {
    if (i != length(clusterTable_kmeans_sub [[writeCluster]])){
      write(paste(clusterTable_kmeans_sub [[writeCluster]][i],"    ",as.character(docs_orig[[clusterTable_kmeans_sub [[writeCluster]][i]]])),file=paste(figPath,"clusters_kmeans_sub.txt",sep=""),append=TRUE)
      write(paste("*",as.character(docs_orig[[clusterTable_kmeans_sub [[writeCluster]][i]]]),"\\newline"),file=paste(figPath,"clusters_kmeans_sub_latex.txt",sep=""),append=TRUE)
      listClusters[i] <- as.character(docs_orig[[clusterTable_kmeans_sub [[writeCluster]][i]]])}
    else if (i == length(clusterTable_kmeans_sub [[writeCluster]])){ 
      write(paste(clusterTable_kmeans_sub [[writeCluster]][i],"    ",as.character(docs_orig[[clusterTable_kmeans_sub [[writeCluster]][i]]])),file=paste(figPath,"clusters_kmeans_sub.txt",sep=""),append=TRUE)
      write(paste("*",as.character(docs_orig[[clusterTable_kmeans_sub [[writeCluster]][i]]]),"\\newline"),file=paste(figPath,"clusters_kmeans_sub_latex.txt",sep=""),append=TRUE)
      listClusters_sub[i] <- as.character(docs_orig[[clusterTable_kmeans_sub [[writeCluster]][i]]])
      bigClusterList_kmeans_sub[[writeCluster]] <- listClusters_sub
      #    dtmCluster <- create_matrix(bigClusterList_kmeans[[writeCluster]],stemWords=TRUE, 
      #                                removeStopwords=TRUE, 
      #                                minWordLength=1,
      #                                removePunctuation= TRUE)
      docsCluster <- Corpus(VectorSource(bigClusterList_kmeans_sub[[writeCluster]]))
      docsCluster <- tm_map(docsCluster, removePunctuation)	#Remove punctuation
      docsCluster <- tm_map(docsCluster, removeNumbers) 	#Remove numbers
      docsCluster <- tm_map(docsCluster, tolower) 			#Force lower case
      docsCluster <- tm_map(docsCluster, stripWhitespace) 	#Remove white space
      
      docsCluster <- tm_map(docsCluster, removeWords, myStopwords)
      #Remove words that only occur once
      docsCluster <- tm_map(docsCluster, removeWords, names(subset(freq_raw_ord,freq_raw_ord==1)))
      #Make plain text
      docsCluster <- tm_map(docsCluster, stemDocument)		#Stem documents
      docsCluster <- tm_map(docsCluster, PlainTextDocument)	
      ##Calculate freqeuncies
      dtmCluster <- DocumentTermMatrix(docsCluster)	#Make document term matrix   
      
      
      
      freq_ordCluster <- sort(colSums(as.matrix(dtmCluster)), decreasing=TRUE)
      mainClusterTerm_kmeans_sub[[writeCluster]] <- head(freq_ordCluster,3)
      #    write("\n",file=paste(figPath,"clusters_kmeans.txt",sep=""),append=TRUE)
      #    write.table(mainClusterTerm_kmeans[[writeCluster]],file=paste(figPath,"clusters_kmeans.txt",sep=""),append=TRUE,col.names = FALSE,quote=FALSE)
      #    write(mainClusterTerm_kmeans[[writeCluster]],file=paste(figPath,"clusters_kmeans.txt",sep=""),append=TRUE)
      inGroup <- which(kmeansSub$cluster==orderClusterSizeSub[writeCluster])
      within <- dtmSubCluster[inGroup,]
      out <- dtmSubCluster[-inGroup,]
      words <- apply(within,2,mean) - apply(out,2,mean)
      labels <- order(words, decreasing=T)[1:3]
      write("\n",file=paste(figPath,"clusters_kmeans_sub.txt",sep=""),append=TRUE)
      write.table(names(words)[labels],file=paste(figPath,"clusters_kmeans_sub.txt",sep=""),append=TRUE,col.names = FALSE,quote=FALSE)
      write("\n",file=paste(figPath,"clusters_kmeans_sub_latex.txt",sep=""),append=TRUE)
      write(paste("\\noindent Number of documents in cluster: ",i,"\\newline",sep=""),file=paste(figPath,"clusters_kmeans_sub_latex.txt",sep=""),append=TRUE)
      write(paste("\\noindent Percentage of corpus: ",format(i/length(doc_labels)*100,digits = 2,nsmall=1),"\\newline",sep=""),file=paste(figPath,"clusters_kmeans_sub_latex.txt",sep=""),append=TRUE)
      write("\n",file=paste(figPath,"clusters_kmeans_sub_latex.txt",sep=""),append=TRUE)
      write("\\noindent\\textbf{Cluster labels:}\\newline",file=paste(figPath,"clusters_kmeans_sub_latex.txt",sep=""),append=TRUE)
      write(paste("\\noindent",names(words)[labels][1]),file=paste(figPath,"clusters_kmeans_sub_latex.txt",sep=""),append=TRUE)
      write("\\newline",file=paste(figPath,"clusters_kmeans_sub_latex.txt",sep=""),append=TRUE)
      write(names(words)[labels][2],file=paste(figPath,"clusters_kmeans_sub_latex.txt",sep=""),append=TRUE)
      write("\\newline",file=paste(figPath,"clusters_kmeans_sub_latex.txt",sep=""),append=TRUE)
      write(names(words)[labels][3],file=paste(figPath,"clusters_kmeans_sub_latex.txt",sep=""),append=TRUE)
      write("\\newline",file=paste(figPath,"clusters_kmeans_sub_latex.txt",sep=""),append=TRUE)
      write("\n",file=paste(figPath,"clusters_kmeans_sub_latex.txt",sep=""),append=TRUE)
  
    }
  }
  
}

clusterTableAll <- append(clusterTable_kmeans[-1],clusterTable_kmeans_sub)
orderClusterSizeAll <- order(sapply(clusterTableAll, length),decreasing=T)
clusterTableAll<-clusterTableAll[orderClusterSizeAll]

bigClusterList_kmeans <- list()
mainClusterTerm_kmeans <- list()
write("",file=paste(figPath,"clusters_kmeans.txt",sep=""),append=FALSE)	#Clear file
write("",file=paste(figPath,"clusters_kmeans_latex.txt",sep=""),append=FALSE)	#Clear file
for (writeCluster in 1:(Nclusters_kmeans+Nclusters_kmeans_sub-1)){
  listClusters <- list()
  write(paste("\n===================\n=====Cluster ",writeCluster,"=====\n===================\n",sep=""),file=paste(figPath,"clusters_kmeans.txt",sep=""),append=TRUE)
  write(paste("\\section*{Cluster ",writeCluster,"}\n",sep=""),file=paste(figPath,"clusters_kmeans_latex.txt",sep=""),append=TRUE)
  for (i in 1:length(clusterTableAll [[writeCluster]])) {
    if (i != length(clusterTableAll [[writeCluster]])){
      write(paste(clusterTableAll [[writeCluster]][i],"    ",as.character(docs_orig[[clusterTableAll [[writeCluster]][i]]])),file=paste(figPath,"clusters_kmeans.txt",sep=""),append=TRUE)
      write(paste("*",as.character(docs_orig[[clusterTableAll [[writeCluster]][i]]]),"\\newline"),file=paste(figPath,"clusters_kmeans_latex.txt",sep=""),append=TRUE)
      listClusters[i] <- as.character(docs_orig[[clusterTableAll [[writeCluster]][i]]])}
    else if (i == length(clusterTableAll [[writeCluster]])){ 
      write(paste(clusterTableAll [[writeCluster]][i],"    ",as.character(docs_orig[[clusterTableAll [[writeCluster]][i]]])),file=paste(figPath,"clusters_kmeans.txt",sep=""),append=TRUE)
      write(paste("*",as.character(docs_orig[[clusterTableAll [[writeCluster]][i]]]),"\\newline"),file=paste(figPath,"clusters_kmeans_latex.txt",sep=""),append=TRUE)
      listClusters[i] <- as.character(docs_orig[[clusterTableAll [[writeCluster]][i]]])
      bigClusterList_kmeans[[writeCluster]] <- listClusters
      
      docsCluster <- Corpus(VectorSource(bigClusterList_kmeans[[writeCluster]]))
      docsCluster <- tm_map(docsCluster, removePunctuation)	#Remove punctuation
      docsCluster <- tm_map(docsCluster, removeNumbers) 	#Remove numbers
      docsCluster <- tm_map(docsCluster, tolower) 			#Force lower case
      docsCluster <- tm_map(docsCluster, stripWhitespace) 	#Remove white space
      
      docsCluster <- tm_map(docsCluster, removeWords, myStopwords)
      #Remove words that only occur once
      docsCluster <- tm_map(docsCluster, removeWords, names(subset(freq_raw_ord,freq_raw_ord==1)))
      #Make plain text
      docsCluster <- tm_map(docsCluster, stemDocument)		#Stem documents
      docsCluster <- tm_map(docsCluster, PlainTextDocument)	
      ##Calculate freqeuncies
      dtmCluster <- DocumentTermMatrix(docsCluster)	#Make document term matrix   
      
      freq_ordCluster <- sort(colSums(as.matrix(dtmCluster)), decreasing=TRUE)
      mainClusterTerm_kmeans[[writeCluster]] <- head(freq_ordCluster,3)
      
      inGroup <- clusterTableAll[[writeCluster]]
      within <- dtm[inGroup,]
      out <- dtm[setdiff(rownames(doc_labels),inGroup),]
      words <- apply(within,2,mean) - apply(out,2,mean)
      labels <- order(words, decreasing=T)[1:3]
      write("\n",file=paste(figPath,"clusters_kmeans.txt",sep=""),append=TRUE)
      write.table(names(words)[labels],file=paste(figPath,"clusters_kmeans.txt",sep=""),append=TRUE,col.names = FALSE,quote=FALSE)
      write("\n",file=paste(figPath,"clusters_kmeans_latex.txt",sep=""),append=TRUE)
      write(paste("\\noindent Number of documents in cluster: ",i,"\\newline",sep=""),file=paste(figPath,"clusters_kmeans_latex.txt",sep=""),append=TRUE)
      write(paste("\\noindent Percentage of corpus: ",format(i/length(doc_labels)*100,digits = 2,nsmall=1),"\\newline",sep=""),file=paste(figPath,"clusters_kmeans_latex.txt",sep=""),append=TRUE)
      write("\n",file=paste(figPath,"clusters_kmeans_latex.txt",sep=""),append=TRUE)
      write("\\noindent\\textbf{Cluster labels:}\\newline",file=paste(figPath,"clusters_kmeans_latex.txt",sep=""),append=TRUE)
      write(paste("\\noindent",names(words)[labels][1]),file=paste(figPath,"clusters_kmeans_latex.txt",sep=""),append=TRUE)
      write("\\newline",file=paste(figPath,"clusters_kmeans_latex.txt",sep=""),append=TRUE)
      write(names(words)[labels][2],file=paste(figPath,"clusters_kmeans_latex.txt",sep=""),append=TRUE)
      write("\\newline",file=paste(figPath,"clusters_kmeans_latex.txt",sep=""),append=TRUE)
      write(names(words)[labels][3],file=paste(figPath,"clusters_kmeans_latex.txt",sep=""),append=TRUE)
      write("\\newline",file=paste(figPath,"clusters_kmeans_latex.txt",sep=""),append=TRUE)
      write("\n",file=paste(figPath,"clusters_kmeans_latex.txt",sep=""),append=TRUE)
      write(paste("\\includegraphics[width=0.6\\linewidth]{./figures/cluster",writeCluster,".pdf}\n",sep=""),file=paste(figPath,"clusters_kmeans_latex.txt",sep=""),append=TRUE)
      
    }
  }
}


###################################
######Analysis of attribtues#######
###################################
attributeHeaders <- read.csv("attribute_headers.csv",sep=",",header=FALSE)
attributesAll = rep(0,26)

for (j in 1:(Nclusters_kmeans+Nclusters_kmeans_sub-1)){
clusterID = j
attributesTotal = rep(0,26)

	for (i in 1:length(clusterTableAll[[clusterID]])){
	attributes <- read.csv(paste('./texts_all/attributes/',clusterTableAll[[clusterID]][i],sep=""),sep=",",header=FALSE)
	attributesTotal <- attributesTotal + as.numeric(attributes)
	attributesAll = attributesAll + + as.numeric(attributes)
	}	#end of i

attributesExpected = rep(sum(attributesTotal)/length(attributesTotal),length(attributesTotal))
Fsig_pos <- 1.96*sqrt(attributesExpected) + attributesExpected
Fsig_neg <- -1.96*sqrt(attributesExpected) + attributesExpected
dfAttributes = data.frame(attributes=attributesTotal,attributesExpected = attributesExpected, label=t(attributeHeaders[1:26]))
dfAttributes$label = factor(dfAttributes$label,levels=dfAttributes[order(dfAttributes$attributes),"label"])		#Set order as a factor, so that we can plot in ascending order

p <- ggplot(data=dfAttributes,aes(x = label,y = attributes)) + geom_bar(stat="identity") 
p <- p + theme_bw() + geom_line(data=dfAttributes,aes(x = 1:26,y = Fsig_pos),colour="red",size=2,linetype="dashed") + geom_line(data=dfAttributes,aes(x = 1:26,y = Fsig_neg),colour="red",size=2,linetype="dashed") 
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1)) + ggtitle(paste("Frequency of attribute use for cluster",clusterID)) + ylab("Count") + xlab("")

pdf(paste(figPath,"clustering_attributes/cluster",clusterID,".pdf",sep=""),width = 12, height = 8)
print(p)
dev.off()

#rm(dfAttributes)
}	#end of j


attributesExpected = rep(sum(attributesAll)/length(attributesAll),length(attributesAll))
Fsig_pos <- 1.96*sqrt(attributesExpected) + attributesExpected
Fsig_neg <- -1.96*sqrt(attributesExpected) + attributesExpected
dfAttributes = data.frame(attributes=attributesAll,attributesExpected = attributesExpected, label=t(attributeHeaders[1:26]))
dfAttributes$label = factor(dfAttributes$label,levels=dfAttributes[order(dfAttributes$attributes),"label"])		#Set order as a factor, so that we can plot in ascending order

write.table(data.frame(attributes=attributesAll,label=t(attributeHeaders[1:26])),file=paste(figPath,"dfattributes",system,".csv"),sep=",",col.names=TRUE,row.names=FALSE)
library(grid)
p <- ggplot(data=dfAttributes,aes(x = label,y = attributes)) + geom_bar(stat="identity") 
p <- p + theme_bw() + geom_line(data=dfAttributes,aes(x = 1:26,y = Fsig_pos),colour="red",size=2,linetype="dashed") + geom_line(data=dfAttributes,aes(x = 1:26,y = Fsig_neg),colour="red",size=2,linetype="dashed")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1,size=rel(2)),axis.text.y=element_text(size=rel(2)),axis.title=element_text(size=rel(2)),plot.margin=unit(c(0.5,0.5,0.5,2),"cm")) + ylab("Count") + xlab("")

pdf(paste(figPath,"clustering_attributes/all.pdf",sep=""),width = 15, height = 10)
print(p)
dev.off()

dataGrouped <- read.csv("attributes_sorted_systems_grouped.csv")
systems <- unique(dataGrouped$system)
for (i in 1:(dim(attributeHeaders)[2]-3)){
dataPlot <- dataGrouped[which(dataGrouped$attribute==as.character(attributeHeaders[[i]])),]

p <- ggplot(dataPlot, aes(attribute, count, fill = system)) + 
  geom_bar(stat="identity", position = "dodge") + theme_bw()
  scale_fill_brewer(palette = "Set1")
  
  pdf(paste(figPath,"clustering_attributes/",as.character(attributeHeaders[[i]]),".pdf",sep=""),width = 12, height = 8)
  print(p)
  dev.off()
  
  pdf(paste(figPath,"clustering_attributes/attribute_",i,".pdf",sep=""),width = 12, height = 8)
  print(p)
  dev.off()
}

#for (i in 1:length(systems))

data2_0 <- dataGrouped[which(dataGrouped$system=='2_0'),]
data5_1 <- dataGrouped[which(dataGrouped$system=='5_1'),]
data9_1 <- dataGrouped[which(dataGrouped$system=='9_1'),]
dataMatrix_2_0 <- dataGrouped[which(dataGrouped$system=='matrix_2_0'),]
dataMatrix_5_1 <- dataGrouped[which(dataGrouped$system=='matrix_5_1'),]


##Synonyms
library("wordnet")
initDict("C:\\Program Files (x86)\\WordNet\\2.1\\dict")
setDict("C:\\Program Files (x86)\\WordNet\\2.1\\dict")
allTerms <- colnames(as.matrix(dtm_raw))

##CUT

# Nclusters = 30		#40 seems to give good results for all
# 
# d_tdm <- dist(t(tdm), method="euclidian")   
# fit_tdm <- hclust(d=d_tdm, method="ward")   
# fit_tdm
# cut_tdm = cutree(fit_tdm,k=Nclusters)
# pdf(paste(figPath,"dend_docs.pdf",sep=""),width = 48, height = 12)
# plot(fit_tdm,hang=-1,cex=1)
# rect.hclust(fit_tdm,k=Nclusters)
# dev.off()
# 
# #Table of clustered terms
# cutMat <- as.matrix(cutree(fit_tdm,k=Nclusters))
# clusterTable <- split(row.names(cutMat),cutMat)
# clusterTable
# 
# bigClusterList <- list()
# mainClusterTerm <- list()
# write("",file=paste(figPath,"clusters.txt",sep=""),append=FALSE)	#Clear file
# for (writeCluster in 1:Nclusters){
#   listClusters <- list()
#   write(paste("\n===================\n=====Cluster ",writeCluster,"=====\n===================\n",sep=""),file=paste(figPath,"clusters.txt",sep=""),append=TRUE)
#   for (i in 1:length(clusterTable [[writeCluster]])) {
#     if (i != length(clusterTable [[writeCluster]])){
#       write(paste(clusterTable [[writeCluster]][i],"    ",as.character(docs_orig[[clusterTable [[writeCluster]][i]]])),file=paste(figPath,"clusters.txt",sep=""),append=TRUE)
#       listClusters[i] <- as.character(docs_orig[[clusterTable [[writeCluster]][i]]])}
#     else if (i == length(clusterTable [[writeCluster]])){ 
#       write(paste(clusterTable [[writeCluster]][i],"    ",as.character(docs_orig[[clusterTable [[writeCluster]][i]]])),file=paste(figPath,"clusters.txt",sep=""),append=TRUE)
#       listClusters[i] <- as.character(docs_orig[[clusterTable [[writeCluster]][i]]])
#       bigClusterList[[writeCluster]] <- listClusters
#       #    dtmCluster <- create_matrix(bigClusterList[[writeCluster]],stemWords=TRUE, 
#       #                                removeStopwords=TRUE, 
#       #                                minWordLength=1,
#       #                                removePunctuation= TRUE)
#       docsCluster <- Corpus(VectorSource(bigClusterList[[writeCluster]]))
#       docsCluster <- tm_map(docsCluster, removePunctuation)	#Remove punctuation
#       docsCluster <- tm_map(docsCluster, removeNumbers) 	#Remove numbers
#       docsCluster <- tm_map(docsCluster, tolower) 			#Force lower case
#       docsCluster <- tm_map(docsCluster, stripWhitespace) 	#Remove white space
#       
#       docsCluster <- tm_map(docsCluster, removeWords, myStopwords)
#       #Remove words that only occur once
#       docsCluster <- tm_map(docsCluster, removeWords, names(subset(freq_raw_ord,freq_raw_ord==1)))
#       #Make plain text
#       docsCluster <- tm_map(docsCluster, stemDocument)		#Stem documents
#       docsCluster <- tm_map(docsCluster, PlainTextDocument)	
#       ##Calculate freqeuncies
#       dtmCluster <- DocumentTermMatrix(docsCluster)	#Make document term matrix   
#       
#       
#       
#       freq_ordCluster <- sort(colSums(as.matrix(dtmCluster)), decreasing=TRUE)
#       mainClusterTerm[[writeCluster]] <- head(freq_ordCluster,5)
#       write("\n",file=paste(figPath,"clusters.txt",sep=""),append=TRUE)
#       write.table(mainClusterTerm[[writeCluster]],file=paste(figPath,"clusters.txt",sep=""),append=TRUE,col.names = FALSE,quote=FALSE)
#       #    write(mainClusterTerm[[writeCluster]],file=paste(figPath,"clusters.txt",sep=""),append=TRUE)
#     }
#   }
#   
# }
# 
# nrow(as.matrix(dtmCluster))/nrow(as.matrix(dtm))