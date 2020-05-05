library(tm)
library(XML)
library(rvest)
library(dplyr)

pos <- "positive-words.txt"
neg <- "negative-words.txt"

p <-scan(pos, character(0), sep = "\n")
n <- scan(neg, character(0), sep = "\n")

head(p,10)
head(n,10)

##================================================
doc.html = htmlTreeParse('http://www.analytictech.com/mb021/mlk.htm', useInternal = TRUE)
doc.text = unlist(xpathApply(doc.html, '//p', xmlValue))
doc.text = gsub ('\\n',' ', doc.text)
doc.text = gsub('\\r', ' ', doc.text)

words.vec <- VectorSource(doc.text)
words.corpus <- Corpus(words.vec)
words.corpus
tdm <- TermDocumentMatrix(words.corpus)
m <- as.matrix(tdm)
wordCounts <- rowSums(m)
wordCounts <- sort(wordCounts, decreasing = TRUE)
head(wordCounts)

##=================================================
totalWords <- sum(wordCounts)
words <- names(wordCounts)

matched <- match(words, p, nomatch = 0)
head(matched, 10)

matched[4]
p[795]
words[9]

##=================================================
mCounts <- wordCounts[which(matched != 0)]
length(mCounts)
mWords <- names(mCounts)
nPos <- sum(mCounts)
nPos
matched <- match(words, n, nomatch = 0)
nCounts <- wordCounts[which(matched != 0)]
nNeg <- sum(nCounts)
nWords <- names(nCounts)
nNeg
length(nCounts)
totalWords <- length(words)
ratioPos <- nPos/totalWords
ratioPos
ratioNeg <- nNeg/totalWords
ratioNeg

##=========================================================
cutpoint <- round(length(words.corpus)/4)

words.corpus1 <- words.corpus[1:cutpoint]
tdm1 <- TermDocumentMatrix(words.corpus1)
m1 <- as.matrix(tdm1)
wordCounts1 <- rowSums(m1)
wordCounts1 <- sort(wordCounts1, decreasing = TRUE)
first25 <- nPos/wordCounts1 
##=========================================================
cutpoint2 <- round(length(words.corpus)/4)

words.corpus2 <- words.corpus[2:cutpoint2]
tdm2 <- TermDocumentMatrix(words.corpus2)
m2 <- as.matrix(tdm2)
wordCounts2 <- rowSums(m2)
wordCounts2 <- sort(wordCounts2, decreasing = TRUE)
second25 <- nPos/wordCounts2

##============================================================
cutpoint3 <- round(length(words.corpus)/4)

words.corpus3 <- words.corpus[3:cutpoint3]
tdm3 <- TermDocumentMatrix(words.corpus3)
m3 <- as.matrix(tdm3)
wordCounts3 <- rowSums(m3)
wordCounts3 <- sort(wordCounts3, decreasing = TRUE)
##===========================================================
cutpoint4 <- round(length(words.corpus)/4)

words.corpus4 <- words.corpus[4:cutpoint4]
tdm4 <- TermDocumentMatrix(words.corpus4)
m4 <- as.matrix(tdm4)
wordCounts4 <- rowSums(m4)
wordCounts4 <- sort(wordCounts4, decreasing = TRUE)

