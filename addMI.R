#Install required packages
packages <- c('tm', 'lexicon', 'tokenizers', 'stringr', 'ds4psy', 'cld2', 'textclean',
              'data.table', 'dplyr', 'stopwords', 'LaF')

for(package in packages){
  if(!(package %in% installed.packages())){
    install.packages(package)
  }
  library(package, character.only = TRUE)
}


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
  corpus <- tm_map(corpus, removePunctuation, ucp=TRUE, preserve_intra_word_contractions=TRUE)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, profanity)
  corpus <- tm_map(corpus, removeWords, "$")
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(str_trim))
  return(corpus)
}



split_into_sentence <- function(preprocessed_text){
  text <- str_split(preprocessed_text, pattern = "ecec ecec")
  text <- str_remove(unlist(text), pattern = "^scsc scsc |^ scsc scsc ")
  text <- text[text != ""]
  text <- str_trim(text)
}


#input : sentence, output : wordpairs
extract_wordpair <- function(sentence, n=3){
  tokens <- unlist(str_split(sentence, "\\s"))
  len = length(tokens)
  pairs <- c()

  if(len >= 4){
    for(i in 1:(len-n)){
      for(j in (i+n):(len)){
        if(tokens[i] == tokens[j]){
          next
        } else {
          pairs[[length(pairs)+1]] <- paste(tokens[i], tokens[j])
        }
      }
    }
    return(pairs)
  }
}


n_line <- 70000
data1 <- readLines("./final/en_US/en_US.blogs.txt", n=n_line)
data2 <- readLines("./final/en_US/en_US.news.txt", n=n_line)
data3 <- readLines("./final/en_US/en_US.twitter.txt", n=n_line)

data <- c(data1, data2, data3)

#Make a corpus
corpus <- VCorpus(VectorSource(data))

corpus <- preprocess_corpus(corpus)

corpus <- tm_map(corpus, content_transformer(split_into_sentence))

textdata <- data.table(text=unlist(sapply(corpus, as.character)))

rm(corpus)                                               
                                               
wordpairs <- apply(textdata, 1, extract_wordpair)

rm(textdata)
                                               
wordpairtable <- data.table(table(unlist(wordpairs)))[order(-N)]

rm(wordpairs)
   
# wordpairtable <- wordpairtable[N > 10]
wordpairtable <- wordpairtable[, c("x", "y") := tstrsplit(V1, "\\s")]
colnames(wordpairtable) <- c('pair', 'c', 'x', 'y')
all_c <- sum(wordpairtable$c)

wordpairtable_cx <- wordpairtable[,.(cx=sum(c)), by=x][order(-cx)]
colnames(wordpairtable_cx) <- c("X", "cx")
wordpairtable_cy <- wordpairtable[,.(cy=sum(c)), by=y][order(-cy)]
colnames(wordpairtable_cy) <- c("Y", "cy")

find_cx <- function(x){
  return()
}

find_cy <- function(y){
  return()
}

get_PMI <- function(x, y, c){
  cx <- wordpairtable_cx[X==x, cx]
  cy <- wordpairtable_cy[Y==y, cy]
  PMI <- log((c*all_c)/(cx*cy))
}

wordpairtable_important <- wordpairtable[!(x %in% stopwords()) & !(y %in% stopwords()) & c > 4 & !(x %in% c("$","-","u")) & !(y %in% c("$","-","u"))]

wordpairtable_important <- wordpairtable_important[, PMI := get_PMI(x, y, c), by=pair]

###############################################################################
#filtering important wordpair(c > 10, PMI >= 0)

wordpairtable_important <- wordpairtable_important[PMI >= 0][order(-PMI)]


