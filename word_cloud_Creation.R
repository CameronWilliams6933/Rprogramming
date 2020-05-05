library(tm)
library(wordcloud)


samplecsv <- read.csv("sample.csv", stringsAsFactors = FALSE, fileEncoding = "latin1")
head(samplecsv)

samplecsv <- samplecsv$text
head(samplecsv)
words.vec <- VectorSource(samplecsv)
words.corpus <- Corpus(words.vec)

words.corpus

##1. Please make sure you should transform the document to lowercase, 
##delete stopwords & numbers & punctuation (1 point).

words.corpus <- tm_map(words.corpus,
                       content_transformer(tolower))
words.corpus <- tm_map(words.corpus,
                       removePunctuation)

words.corpus <- tm_map(words.corpus,removeNumbers)

##2. Add "can" and "just" to the stopwords list (1 point).
words.corpus <- tm_map(words.corpus, removeWords,c(stopwords("english"),"can", "just"))


tdm <- TermDocumentMatrix(words.corpus)
tdm


m <- as.matrix(tdm)


wordcounts <- rowSums(m)
wordcounts <- sort(wordcounts, decreasing = TRUE)


cloudFrame <- data.frame( word = names(wordcounts),freq = wordcounts)

##3. Use five colors and "Accent" for color theme (1 point).
wordcloud(cloudFrame$word, cloudFrame$freq)
wordcloud(names(wordcounts), wordcounts, min.freq = 1,
          max.words = 200, random.order = FALSE, rot.per = 0.35., colors = brewer.pal(5,"Accent"))

##4. Does the word cloud convey the key points of the document? (1 point). 
##I feel that the word cloud conveys the words that were frequently used and the ones that were not,
##in my cloud it conveys that "virginamerica" was the word that was talked about most frequently. While
##Second most frequent word used was "flight". 

##5. Submit the compiled file (docx). 
##The wordcloud may prevent you from compiling, if so, do the following: 

jpeg('p1.jepg')
wordcloud(cloudFrame$word, cloudFrame$freq)
dev.off()
          