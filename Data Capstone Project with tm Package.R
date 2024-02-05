#Install required packages
packages <- c('tm','readr','stringi','tokenizers','ggplot2','wordcloud','SnowballC','gridExtra', 'sentimentr', 
              'textstem', 'lexicon', 'stringr', 'dplyr', 'data.table')

for(package in packages){
  if(!(package %in% installed.packages())){
    install.packages(package)
  }
  library(package, character.only = TRUE)
}

#Data install from blog, news, twitter
data1 <- readLines("./final/en_US/en_US.blogs.txt", n=300)
data2 <- readLines("./final/en_US/en_US.news.txt", n=300)
data3 <- readLines("./final/en_US/en_US.twitter.txt", n=300)

data <- c(data1, data2, data3)

#Make a corpus
corpus <- VCorpus(VectorSource(data))

#Data preprocessing
##1. to lower
corpus <- tm_map(corpus, content_transformer(tolower))
#tm_map allow function that is applied to vector or textdocument also to be able to applied to corpus.

##2. remove punctuations
corpus <- tm_map(corpus, removePunctuation)

##3. remove numbers
corpus <- tm_map(corpus, removeNumbers)

##4. remove profanity
pro1 <- profanity_banned
pro2 <- profanity_arr_bad
pro3 <- profanity_racist

profanity <- union(union(pro1, pro2),pro3)

corpus <- tm_map(corpus, removeWords, profanity)

##4.5. remove foreign language 

##5. remove extra white space
corpus <- tm_map(corpus, stripWhitespace)


##6. lemmatization
corpus <- tm_map(corpus, content_transformer(lemmatize_strings))
#content_transformer allow function applied to only vector to be able to be applied to textdocument.
#after when i need, i can modify the lemmatize dictionary by using hash_lemmas(lexicon)

#Check the corpus after preprocess
for(i in 1:6){
  print(corpus[[i]]$content)
}

#Preprocessing is done.

#Make n-grams(n=1~3)
dataforngrams <- data.frame(text=sapply(corpus, as.character), stringsAsFactors = FALSE)
unigramtoken <- apply(dataforngrams, 1, tokenize_ngrams, n=1, n_min=1)
bigramtoken <- apply(dataforngrams, 1, tokenize_ngrams, n=2, n_min=2)
trigramtoken <- apply(dataforngrams, 1, tokenize_ngrams, n=3, n_min=3)

unigrams <- data.frame(table(unlist(unigramtoken)))
colnames(unigrams) <- c('word', 'frequency')
unigrams <- unigrams[order(unigrams$freq, decreasing = TRUE),]

bigrams <- data.frame(table(unlist(bigramtoken)))
colnames(bigrams) <- c('word', 'frequency')
bigrams <- bigrams[order(bigrams$freq, decreasing = TRUE),]

trigrams <- data.frame(table(unlist(trigramtoken)))
colnames(trigrams) <- c('word', 'frequency')
trigrams <- trigrams[order(trigrams$freq, decreasing = TRUE),]

threeGramTable <- trigrams
threeGramTable <- threeGramTable %>% mutate(firstTerms=paste(str_split_i(word, "\\s", i=1),
                                          str_split_i(word, "\\s", i=2), sep = "_")) %>% 
  mutate(lastTerm=str_split_i(word, "\\s", i=3)) %>%
  select(frequency:lastTerm)
threeGramTable <- as.data.table(threeGramTable)

#Modeling
#Is there any package dealing with katz's backoff model?
#https://github.com/ThachNgocTran/KatzBackOffModelImplementationInR/blob/master/calculateDiscount.R

#add discount rate and leftoverprob column
calcLeftOverProb = function(lastTerm, frequency, discount){
  all_freq = sum(frequency)
  
  return(1-sum((discount*frequency)/all_freq))
}


createThreeGramTableExtended = function(){
  threeGramTable$discount = rep(1, nrow(threeGramTable))
  
  for(i in 5:1){
    currRTimes = i
    nextRTimes = currRTimes + 1
    
    currN = nrow(threeGramTable[frequency == currRTimes])
    nextN = nrow(threeGramTable[frequency == nextRTimes])
    
    currd = (nextRTimes / currRTimes) * (nextN / currN) # assumption: 0 < d < 1
    
    # the beauty of "data.table"!
    threeGramTable[frequency == currRTimes, discount := currd]
  }
  
  # Calculate the remaining probability (thanks to discounting...).
  threeGramTable_leftOverProb = threeGramTable[, .(leftoverprob=calcLeftOverProb(lastTerm, frequency, discount)), by=firstTerms]
  
  # We now have two important objects: threeGramTable, threeGramTable_leftOverProb
  # ...
}

##########################################################################################################
# Calculate the remaining probability (thanks to discounting...).
# Given that the input is grouped based on firstTerms already.


