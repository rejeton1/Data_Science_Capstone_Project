install.packages(c('tm','readr','stringi','tokenizers','ggplot2','wordcloud','SnowballC','gridExtra', 'sentimentr'))
for(package in c('tm','readr','stringi','tokenizers','ggplot2','wordcloud','SnowballC','gridExtra', 'sentimentr')){
  library(package, character.only = TRUE)
}

data <- readLines("./final/en_US/en_US.blogs.txt", n=1000)

corpus <- VCorpus(VectorSource(data))

corpus <- tm_map(corpus, content_transformer(tolower))

corpus <- tm_map(corpus, removePunctuation)

corpus <- tm_map(corpus, removeNumbers)

corpus <- tm_map(corpus, stripWhitespace)




pro1 <- lexicon::profanity_banned
pro2 <- lexicon::profanity_arr_bad
pro3 <- lexicon::profanity_racist

profanity <- union(union(pro1, pro2),pro3)

#remove profanity
corpus <- tm_map(corpus, removeWords, profanity)


#stemming
corpusstemmed <- tm_map(corpus, stemDocument)

#searching specific string in corpus
filtered.corpus <- tm_filter(corpus, FUN = function(x) any(grep("of the", content(x))))

corpus <- tm_map(corpus, PlainTextDocument)

#n-gram create
dataforngrams <- data.frame(text=sapply(corpus, as.character), stringsAsFactors = FALSE)
unigramtoken <- apply(dataforngrams, 1, tokenize_ngrams, n=1, n_min=1)
bigramtoken <- apply(dataforngrams, 1, tokenize_ngrams, n=2, n_min=2)
trigramtoken <- apply(dataforngrams, 1, tokenize_ngrams, n=3, n_min=3)

unigrams <- data.frame(table(unlist(unigramtoken)))
unigrams <- unigrams[order(unigrams[,2], decreasing = TRUE),]
colnames(unigrams) <- c('Word', 'Frequency')

bigrams <- data.frame(table(unlist(bigramtoken)))
bigrams <- bigrams[order(bigrams[,2], decreasing = TRUE),]
colnames(bigrams) <- c('Word', 'Frequency')

trigrams <- data.frame(table(unlist(trigramtoken)))
trigrams <- trigrams[order(trigrams[,2], decreasing = TRUE),]
colnames(trigrams) <- c('Word', 'Frequency')

#make wordcloud
dtmcorpus <- TermDocumentMatrix(corpus)
corpusmatrix <- as.matrix(dtmcorpus)
sortedmatrix <- sort(rowSums(corpusmatrix), decreasing = TRUE)
dfcorpus <- data.frame(word=names(sortedmatrix), freq=sortedmatrix)
wordcloud(words = dfcorpus$word, freq = dfcorpus$freq, min.freq = 1, max.words = 50, random.order = FALSE,
          rot.per = 0.35, colors = brewer.pal(n=8, name='Dark2'))















