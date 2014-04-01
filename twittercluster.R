library(tseries)
library(ggplot2)
library(forecast)
library(maps)
library(XML)
library(quantmod) #use getSymbols.yahoo, gets em into default namespace
library(fImport)  #use yahooSeries, 	 lets you name them... 
library(reshape)  #for rename
library(sp)
library(maptools)
library(rgdal)
library(RColorBrewer)
library(classInt)
library(gridExtra)
library(lattice)
library(gdata)
library(twitteR)
library(tm)
library(ggdendro)

###
### Read tweets from Twitter using ATOM (XML) format
### http://heuristically.wordpress.com/2011/04/08/text-data-mining-twitter-r/
### http://jeffreybreen.wordpress.com/2011/07/04/twitter-text-mining-r-slides/
### http://www.r-bloggers.com/ggheat-a-ggplot2-style-heatmap-function/
 
# initialize a storage variable for Twitter tweets
mydata.vectors <- character(0)
 
# paginate to get more tweets
for (page in c(1:15))
{
    # search parameter
#    twitter_q <- URLencode('#inflation OR #unemployment')
    twitter_q <- URLencode('#health OR #healthcare OR #hospital')
    # construct a URL
    twitter_url = paste('http://search.twitter.com/search.atom?q=',twitter_q,'&rpp=100&page=', page, sep='')
    # fetch remote URL and parse
    mydata.xml <- xmlParseDoc(twitter_url, asText=F)
    # extract the titles
    mydata.vector <- xpathSApply(mydata.xml, '//s:entry/s:title', xmlValue, namespaces =c('s'='http://www.w3.org/2005/Atom'))
    # aggregate new tweets with previous tweets
    mydata.vectors <- c(mydata.vector, mydata.vectors)
}
 
# how many tweets did we get?
length(mydata.vectors)


#####################
 
# build a corpus
mydata.corpus <- Corpus(VectorSource(mydata.vectors))
 
# make each letter lowercase
mydata.corpus <- tm_map(mydata.corpus, tolower)
 
# remove punctuation
mydata.corpus <- tm_map(mydata.corpus, removePunctuation)
 
# remove generic and custom stopwords
my_stopwords <- c(stopwords('english'), 'health', 'via')
#my_stopwords <- c(stopwords('english'))
mydata.corpus <- tm_map(mydata.corpus, removeWords, my_stopwords)
 
# build a term-document matrix
mydata.dtm <- TermDocumentMatrix(mydata.corpus)
 
# inspect the document-term matrix
mydata.dtm
 
# inspect most popular words
findFreqTerms(mydata.dtm, lowfreq=30)


#################

#findAssocs(mydata.dtm, 'fed', 0.20)

#################

# remove sparse terms to simplify the cluster plot
# Note: tweak the sparse parameter to determine the number of words.
# About 10-30 words is good.
#mydata.dtm2 <- removeSparseTerms(mydata.dtm, sparse=0.95)
#mydata.dtm2 <- removeSparseTerms(mydata.dtm, sparse=0.2)
mydata.dtm2 <- removeSparseTerms(mydata.dtm, sparse=0.965)
 
# convert the sparse term-document matrix to a standard data frame
mydata.df <- as.data.frame(inspect(mydata.dtm2))
 
# inspect dimensions of the data frame
nrow(mydata.df)
ncol(mydata.df)

####################

mydata.df.scale <- scale(mydata.df)
d <- dist(mydata.df.scale, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward")

png('health.png')
plot(fit) # display dendogram? 
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=5, border="red")
dev.off()

q <- ggdendrogram(fit, k=5, boarder="red") + theme_rect(groups)
ggsave('ggd.png', plot=q, pointsize=10)

