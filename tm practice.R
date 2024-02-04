install.packages(c('tm','readr','stringi','RWeka','ggplot2','wordcloud','SnowballC','gridExtra', 'sentimentr'))
for(package in c('tm','readr','stringi','RWeka','ggplot2','wordcloud','SnowballC','gridExtra', 'sentimentr')){
  library(package, character.only = TRUE)
}

data <- readLines("./final/en_US/en_US.blogs.txt", n=1000)

corpus <- VCorpus(VectorSource(data))

corpus <- tm_map(corpus, content_transformer(tolower))

corpus <- tm_map(corpus, removePunctuation)

corpus <- tm_map(corpus, stripWhitespace)

corpus <- tm_map(corpus, removeNumbers)


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


#n-gram create
dataforngrams <- data.frame(text=sapply(corpus, as.character), stringsAsFactors = FALSE)
# unigramToken

Sys.setenv('JAVA_HOME'="C:/Program Files (x86)/Java/jre-1.8/") 




