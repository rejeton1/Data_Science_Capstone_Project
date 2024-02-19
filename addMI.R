#Install required packages
packages <- c('tm', 'lexicon', 'tokenizers', 'stringr', 'ds4psy', 'cld2', 'textclean',
              'data.table', 'dplyr', 'stopwords', 'LaF')

for(package in packages){
  if(!(package %in% installed.packages())){
    install.packages(package)
  }
  library(package, character.only = TRUE)
}


n_line <- 10
data1 <- readLines("./final/en_US/en_US.blogs.txt", n=n_line)
data2 <- readLines("./final/en_US/en_US.news.txt", n=n_line)
data3 <- readLines("./final/en_US/en_US.twitter.txt", n=n_line)

data <- c(data1, data2, data3)

#Make a corpus
corpus <- VCorpus(VectorSource(data))

################################################################################
#Data preprocessing

pro1 <- profanity_banned
pro2 <- profanity_arr_bad
pro3 <- profanity_racist

profanity <- union(union(pro1, pro2),pro3)

add_start_end_token <- function(text){
  sentences <- text_to_sentences(text)
  added_sentences <- paste("scsc scsc", sentences, "ecec ecec")
  sum_up_sentences <- paste(added_sentences, collapse = " ")
  return(sum_up_sentences)
}

removeQuotation <- function(x){
  x <- gsub("“","",x,fixed = TRUE)
  x <- gsub("”","",x,fixed = TRUE)
  return(x)
} 

preprocess_corpus <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(removeQuotation))
  corpus <- tm_map(corpus, content_transformer(function(x) add_start_end_token(x)))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, content_transformer(replace_non_ascii))
  corpus <- tm_map(corpus, removePunctuation, ucp=TRUE, preserve_intra_word_contractions=TRUE,
                   preserve_intra_word_dashes=TRUE)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, profanity)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(function(x) str_remove(x, pattern="\\s$")))
  corpus <- tm_map(corpus, content_transformer(function(x) str_remove(x, pattern="^\\s")))
  return(corpus)
}


corpus <- preprocess_corpus(corpus)


split_into_sentence <- function(preprocessed_text){
  text <- str_split(preprocessed_text, pattern = "ecec ecec")
  text <- str_remove(unlist(text), pattern = "^scsc scsc |^ scsc scsc ")
  text <- text[text != ""]
  text <- str_trim(text)
}

corpus <- tm_map(corpus, content_transformer(split_into_sentence))

textdata <- data.table(text=unlist(sapply(corpus, as.character)))

#input : sentence, output : wordpairs
extract_wordpair <- function(sentence, n=3){
  tokens <- unlist(str_split(sentence, "\\s"))
  len = length(tokens)
  pairs <- c()

  if(len >= 4){
    for(i in 1:(len-n)){
      for(j in (i+n):(len)){
        pairs[length(pairs)+1] <- paste(tokens[i], tokens[j])
      }
    }
    return(pairs)
  }
}







