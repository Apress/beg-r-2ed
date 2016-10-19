##Determining R version, Machine type, etc
R.Version()
Sys.info()
##########################################
##tm does text mining
install.packages("tm")
library(tm)

##word cloud (obviously)
install.packages("wordcloud")
library(wordcloud)

##mining news on the web
install.packages("tm.plugin.webmining")
library(tm.plugin.webmining)

##latent dirichlet allocation model with VEM or Gibbs Sampling for words
##topic model paper https://www.cs.princeton.edu/~blei/papers/Blei2012.pdf
install.packages("topicmodels")
library(topicmodels)

##word stems
install.packages("SnowballC")
library(SnowballC)
################Early Visualisation and tm package############
pdf(file = "chapter19_%03d.pdf", width = 7, height = 7, onefile = FALSE)

austen <- Corpus (DirSource("ch19/", pattern="pg1342"))
inspect(austen)
summary(austen)

wordcloud(austen, scale=c(5,0.5), max.words=100, random.order=FALSE,
          rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))


austen <- tm_map(austen, content_transformer(tolower))
austen <- tm_map(austen, removePunctuation)
austen <- tm_map(austen, removeWords, stopwords("english"))
austen <- tm_map(austen, content_transformer(stripWhitespace))

wordcloud(austen, scale=c(5,0.5), max.words=100, random.order=FALSE,
          rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))

austen <- tm_map(austen, stemDocument)

wordcloud(austen, scale=c(5,0.5), max.words=100, random.order=FALSE,
          rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))

########################################
###We may wish to transform many such a Corpus.  Make a function!
txttrans = function(text){
  text = tm_map(text, content_transformer(tolower))
  text = tm_map(text, removePunctuation)
  ##text = tm_map(text, content_transformer(removeNumbers))
  text = tm_map(text, removeWords, stopwords("english"))
  text = tm_map(text, content_transformer(stripWhitespace))
  ##text = tm_map(text, stemDocument)
  text
}

####term documentmatrix
austen = TermDocumentMatrix(austen)
austen
findFreqTerms(austen, low = 100)



################################
##PDFs
austen2<-Corpus(DirSource("ch19/", pattern="pdf"), readerControl = list(reader=readPDF))
austen2 <- txttrans(austen2)
summary(austen2)
wordcloud(austen2, scale=c(5,0.5), max.words=100, random.order=FALSE,
          rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
###########################################
##Web
austen4 = WebCorpus(GoogleNewsSource("Jane Austen"))
austen4
austen4 = txttrans(austen4)
wordcloud(austen4, scale=c(5,0.5), max.words=100, random.order=FALSE,
          rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))

#############################################################################
## now let's take a look at several books at once
austen_pesm <- Corpus(DirSource("ch19/", pattern="pg"))
summary((austen_pesm))

########################################
### add stemming again
txttrans = function(text){
  text = tm_map(text, content_transformer(tolower))
  text = tm_map(text, removePunctuation)
  text = tm_map(text, content_transformer(removeNumbers))
  text = tm_map(text, removeWords, stopwords("english"))
  text = tm_map(text, content_transformer(stripWhitespace))
  text = tm_map(text, stemDocument)
  text
}

austen_pesm = txttrans(austen_pesm)
wordcloud(austen_pesm, scale=c(5,0.5), max.words=100, random.order=FALSE,
          rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))

austen_pesm_DTM = DocumentTermMatrix(austen_pesm)
findFreqTerms(austen_pesm_DTM, low = 400)

austen_a = findAssocs(austen_pesm_DTM, terms = c("elizabeth", "emma", "miss"), corlim =  c(0.85, 0.90, 0.95))
austen_a$miss


## Rgraphviz package required for plotting
source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")

## make the plot
plot(austen_pesm_DTM, terms = findFreqTerms(austen_pesm_DTM, lowfreq = 400),
     corThreshold = 0.65)

## clean it up restricting low frequency words
plot(austen_pesm_DTM, terms = findFreqTerms(austen_pesm_DTM, lowfreq = 800),
     corThreshold = 0.65)


## clean it up restricting low frequency words and increasing correlation threshold
plot(austen_pesm_DTM, terms = findFreqTerms(austen_pesm_DTM, lowfreq = 850),
     corThreshold = 0.95)


##topic model##
austen_pesm_DTM

##remove documents with zero length
rowTotals <- apply(austen_pesm_DTM, 1, sum)
austen_pesm_DTM<-austen_pesm_DTM[rowTotals>0,]


k <- 2
austen_pesm_lda <- LDA(austen_pesm_DTM, control = list(alpha=0.2, nstart = 100), k)
topics(austen_pesm_lda)
terms(austen_pesm_lda, 5)
dev.off()
