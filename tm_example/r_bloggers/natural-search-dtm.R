#### 0. Setup
library("RSiteCatalyst")
library("RTextTools") #Loads many packages useful for text mining

#### 1. RSiteCatalyst code - Get Natural Search Keywords & Metrics

#Set credentials
SCAuth(<username:company>, <shared secret>)

#Get list of search engine terms
searchkeywords <- QueueRanked(<report_suite>, "2013-02-01","2013-09-16", 
                  c("entries", "visits", "pageviews", "instances", "bounces"), 
                  "searchenginenaturalkeyword", top="100000", startingWith = "1")

#### 2. Process keywords into format suitable for text mining

#Create document-term matrix, passing data cleaning options
#Stem the words to avoid multiples of similar words
#Need to set wordLength to minimum of 1 because "r" a likely term
dtm <- create_matrix(searchkeywords$'Natural Search Keyword', 
                     stemWords=TRUE, 
                     removeStopwords=FALSE, 
                     minWordLength=1,
                     removePunctuation= TRUE)