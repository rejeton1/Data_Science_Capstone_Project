#Install required packages
packages <- c('tm', 'lexicon', 'tokenizers', 'stringr', 'ds4psy', 'cld2', 'textclean',
              'data.table', 'dplyr', 'stopwords', 'LaF')

for(package in packages){
  if(!(package %in% installed.packages())){
    install.packages(package)
  }
  library(package, character.only = TRUE)
}


#Data install from blog, news, twitter
n_line <- 50000
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


cleaned_corpus <- preprocess_corpus(corpus)

before_after_process <- function(num){
  before_after_corpus <- c(corpus[[num]]$content, cleaned_corpus[[num]]$content)
  return(before_after_corpus)
}


#################################################################################
#Make n-grams(n=1~3)
dataforngrams <- data.frame(text=sapply(cleaned_corpus, as.character), 
                            stringsAsFactors = FALSE)
unigramtoken <- apply(dataforngrams, 1, tokenize_ngrams, n=1, n_min=1)
bigramtoken <- apply(dataforngrams, 1, tokenize_ngrams, n=2, n_min=2)
trigramtoken <- apply(dataforngrams, 1, tokenize_ngrams, n=3, n_min=3)

oneGramTable <- data.table(table(unlist(unigramtoken)))[order(-N)]
colnames(oneGramTable) <- c('lastTerm', 'frequency')

twoGramTable <- data.table(table(unlist(bigramtoken)))
twoGramTable <- twoGramTable[,.(firstTerms=str_split_i(V1, "\\s", i=1),
                                lastTerm=str_split_i(V1,"\\s", i=2), N)][order
                                                                         (-N)]
colnames(twoGramTable)[3] <- 'frequency'


threeGramTable <- data.table(table(unlist(trigramtoken)))
threeGramTable <- threeGramTable[,.(firstTerms=paste(str_split_i(V1, "\\s", i=1),
                                                     str_split_i(V1, "\\s", i=2), 
                                                     sep = "_"),
                                    lastTerm=str_split_i(V1, "\\s", i=3),N)][order
                                                                             (-N)]
colnames(threeGramTable)[3] <- 'frequency'
#################################################################################
##########################################
calcLeftOverProb = function(lastTerm, frequency, discount){
  all_freq = sum(frequency)
  
  return(1-sum((discount*frequency)/all_freq))
}

#add discount column to onegramtable
oneGramTable$discount = rep(1, nrow(oneGramTable))

for(iii in 5:1){
  currRTimes = iii
  nextRTimes = currRTimes + 1
  
  currN = nrow(oneGramTable[frequency == currRTimes])
  nextN = nrow(oneGramTable[frequency == nextRTimes])
  
  currd = (nextRTimes / currRTimes) * (nextN / currN) # assumption: 0 < d < 1
  
  # the beauty of "data.table"!
  oneGramTable[frequency == currRTimes, discount := currd]
}

oneGramTable <- mutate(oneGramTable, c_est=frequency*discount)
oneGramTable <- oneGramTable[order(-c_est)]
################################################
#add discount column to twogramtable
twoGramTable$discount = rep(1, nrow(twoGramTable))

for(ii in 5:1){
  currRTimes = ii
  nextRTimes = currRTimes + 1
  
  currN = nrow(twoGramTable[frequency == currRTimes])
  nextN = nrow(twoGramTable[frequency == nextRTimes])
  
  currd = (nextRTimes / currRTimes) * (nextN / currN) # assumption: 0 < d < 1
  
  # the beauty of "data.table"!
  twoGramTable[frequency == currRTimes, discount := currd]
}

twoGramTable_leftOverProb = twoGramTable[, .(leftoverprob=calcLeftOverProb(lastTerm, frequency, discount)), by=firstTerms]

twoGramTable <- mutate(twoGramTable, c_est=frequency*discount)
twoGramTable <- twoGramTable[order(-c_est)]
#add discount rate and leftoverprob column


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
threeGramTable <- mutate(threeGramTable, c_est=frequency*discount)
threeGramTable <- threeGramTable[order(-c_est)]

##########################################################################################################
# This function is used to get the probability of a given text, using Katz Backoff (with Good-Turing Discounting).
# Only consider the last 3 words.
# Input1: I want to sleep
# Output1: 0.04536
# Input2: I want to go
# Output2: 0.4323
# Thus, input2 has more chance to appear than input1. Statistically, it is more relevant to people.
getProbabilityFrom3Gram = function(inputString){
  # Preprocessing
  mylist = separateTerms(getLastTerms(inputString, num = 3))
  inFirstTerms3gram = mylist$firstTerms
  inLastTerm3gram = mylist$lastTerm
  
  finalProb = -1
  
  oneGroupIn3Gram = threeGramTable[firstTerms == inFirstTerms3gram]
  if (nrow(oneGroupIn3Gram) > 0){
    # Algorithm here
    oneRecordIn3Gram = threeGramTable[firstTerms == inFirstTerms3gram & lastTerm == inLastTerm3gram]
    if (nrow(oneRecordIn3Gram) > 0){
      # We found one in 3-gram
      all_freq = sum(oneGroupIn3Gram$frequency)
      finalProb = ((oneRecordIn3Gram$discount * oneRecordIn3Gram$frequency) / all_freq)
      ### We're done!
    } else {
      # NOT found in 3-gram => check 2-gram & 1-gram
      mylist = separateTerms(getLastTerms(inputString, num = 2))
      inFirstTerms2gram = mylist$firstTerms
      inLastTerm2gram = mylist$lastTerm
      
      # Get the left-over probability so that we can distribute it for lower-order grams.
      beta_leftoverprob = threeGramTable_leftOverProb[firstTerms == inFirstTerms3gram]$leftoverprob
      
      oneGroupIn2Gram = twoGramTable[firstTerms == inFirstTerms2gram]
      oneRecordIn2Gram = twoGramTable[firstTerms == inFirstTerms2gram & lastTerm == inLastTerm2gram]
      if (nrow(oneRecordIn2Gram) > 0){
        # We found one in 2-gram!
        # We only consider ones that do not appear in 3-grams...
        oneGroupIn2Gram_Remain = oneGroupIn2Gram[!(oneGroupIn2Gram$lastTerm %in% oneGroupIn3Gram$lastTerm)]
        all_freq = sum(oneGroupIn2Gram$frequency)
        
        alpha = beta_leftoverprob / sum((oneGroupIn2Gram_Remain$frequency * oneGroupIn2Gram_Remain$discount) / all_freq)
        
        finalProb = alpha * ((oneRecordIn2Gram$frequency * oneRecordIn2Gram$discount ) / all_freq)
        ### We're done!
      } else {
        # We only have hope in 1-gram!
        oneGroupIn1Gram = oneGramTable # we don't have "firstTerms" here!
        oneRecordIn1Gram = oneGramTable[lastTerm == inLastTerm2gram] # what if this returns "zero" row?
        
        oneGroupIn1Gram_Remain = oneGroupIn1Gram[!(oneGroupIn1Gram$lastTerm %in% oneGroupIn3Gram$lastTerm)]
        all_freq = sum(oneGroupIn1Gram$frequency)
        
        alpha = beta_leftoverprob / sum((oneGroupIn1Gram_Remain$frequency * oneGroupIn1Gram_Remain$discount) / all_freq)
        
        finalProb = alpha * ((oneRecordIn1Gram$frequency * oneRecordIn1Gram$discount) / all_freq)
        ### We're done!
      }
    }
  } else {
    #ABX doesn't exist at all
    mylist = separateTerms(getLastTerms(inputString, num = 2))
    inFirstTerms2gram = mylist$firstTerms #B
    inLastTerm2gram = mylist$lastTerm     #C
    
    
    oneGroupIn2Gram = twoGramTable[firstTerms == inFirstTerms2gram]   #BX
    oneRecordIn2Gram = twoGramTable[firstTerms == inFirstTerms2gram & lastTerm == inLastTerm2gram]    #BC
    
    if(nrow(oneGroupIn2Gram) > 0){
      beta_leftoverprob = twoGramTable_leftOverProb[firstTerms == inFirstTerms2gram]$leftoverprob
      if (nrow(oneRecordIn2Gram) > 0){
        all_freq = sum(oneGroupIn2Gram$frequency)
        finalProb = ((oneRecordIn2Gram$discount * oneRecordIn2Gram$frequency) / all_freq)
      } else {
        oneGroupIn1Gram = oneGramTable # we don't have "firstTerms" here!
        oneRecordIn1Gram = oneGramTable[lastTerm == inLastTerm2gram] # what if this returns "zero" row?
        
        oneGroupIn1Gram_Remain = oneGroupIn1Gram[!(oneGroupIn1Gram$lastTerm %in% oneGroupIn2Gram$lastTerm)]
        all_freq = sum(oneGroupIn1Gram$frequency)
        
        alpha = beta_leftoverprob / sum((oneGroupIn1Gram_Remain$frequency * oneGroupIn1Gram_Remain$discount) / all_freq)
        
        finalProb = alpha * ((oneRecordIn1Gram$frequency * oneRecordIn1Gram$discount) / all_freq)
     }
    } else {
      oneGroupIn1Gram = oneGramTable
      oneRecordIn1Gram = oneGramTable[lastTerm == inLastTerm2gram]
      all_freq = sum(oneGroupIn1Gram$frequency)
      finalProb = ((oneRecordIn1Gram$discount * oneRecordIn1Gram$frequency) / all_freq)
    }
  }
  finalProb
}


################################################################################
# This function is used to extract terms.
# Input: "A_B_C"
#        "X_Y_Z"
# Output: firstTerms  lastTerm
#         "A_B"       "C"
#         "X_Y"       "Z"
separateTerms = function(x){
  # Pre-allocate
  firstTerms = character(length(x))
  lastTerm = character(length(x))
  
  for(i in 1:length(x)){
    posOfSpaces = gregexpr("_", x[i])[[1]]
    posOfLastSpace = posOfSpaces[length(posOfSpaces)]
    firstTerms[i] = substr(x[i], 1, posOfLastSpace-1)
    lastTerm[i] = substr(x[i], posOfLastSpace+1, nchar(x[i]))
  }
  
  list(firstTerms=firstTerms, lastTerm=lastTerm)
}


################################################################################
# This function is used to get the last "num" terms of a given text.
# Input: We are students of the university
# Output: of_the_university (if num = 3)
getLastTerms = function(inputString, num = 3){
  # Preprocessing
  inputString = gsub("[[:space:]]+", " ", str_trim(tolower(inputString)))
  
  # Now, ready!
  words = unlist(strsplit(inputString, " "))
  
  if (length(words) < num){
    stop("Number of Last Terms: Insufficient!")
  }
  
  from = length(words)-num+1
  to = length(words)
  tempWords = words[from:to]
  
  paste(tempWords, collapse="_")
}


#########################################
#Make predict function(This function predict probability of next words only given lastTerm. Through 3-gram Katz's model.)

predictnextword <- function(inputtext){
  inputtext_processed1 <- getLastTerms(inputtext, n=2)
  inputtext_processed2 <- getLastTerms(inputtext, n=1)
  
  threeGramTable1 <- threeGramTable[firstTerms==inputtext_processed1]
  twoGramTable2 <- twoGramTable[firstTerms==inputtext_processed2]
  
  if(dim(threeGramTable1)[1] > 0){
    if(dim(threeGramTable1)[1] >= 20){
      threeprob <- threeGramTable1[1:20,.(probability=getProbabilityFrom3Gram(paste(inputtext, as.character(lastTerm)))),by=lastTerm][order(-probability)]
    }else{
      threeprob <- threeGramTable1[,.(probability=getProbabilityFrom3Gram(paste(inputtext, as.character(lastTerm)))),by=lastTerm][order(-probability)]
    }
    
    twoGramTable1 <- twoGramTable[firstTerms==inputtext_processed2 & !(lastTerm %in% threeGramTable1$lastTerm)]
    
    if(dim(twoGramTable1)[1] >= 20){
      twoprob <- twoGramTable1[1:20,.(probability=getProbabilityFrom3Gram(paste(inputtext, as.character(lastTerm)))),by=lastTerm][order(-probability)]
    }else{
      twoprob <- twoGramTable1[,.(probability=getProbabilityFrom3Gram(paste(inputtext, as.character(lastTerm)))),by=lastTerm][order(-probability)]
    }
    
    oneGramTable1 <- oneGramTable[!(lastTerm %in% twoGramTable1$lastTerm) & !(lastTerm %in% threeGramTable1$lastTerm)]
    
    if(dim(oneGramTable1)[1] >= 20){
      oneprob <- oneGramTable1[1:20,.(probability=getProbabilityFrom3Gram(paste(inputtext, as.character(lastTerm)))),by=lastTerm][order(-probability)]
    }else{
      oneprob <- oneGramTable1[,.(probability=getProbabilityFrom3Gram(paste(inputtext, as.character(lastTerm)))),by=lastTerm][order(-probability)]
    }
    
    finalprob <- rbind(threeprob, twoprob, oneprob)
    finalprob <- finalprob[!(lastTerm %in% stopwords()) & !(lastTerm %in% c("scsc", "ecec"))][order(-probability)]
    
    
  } else if(dim(twoGramTable2)[1] > 0){
    
    if(dim(twoGramTable2)[1] >= 20){
      twoprob <- twoGramTable2[1:20,.(probability=getProbabilityFrom3Gram(paste(inputtext, as.character(lastTerm)))),by=lastTerm][order(-probability)]
    }else{
      twoprob <- twoGramTable2[,.(probability=getProbabilityFrom3Gram(paste(inputtext, as.character(lastTerm)))),by=lastTerm][order(-probability)]
    }
    
    oneGramTable2 <- oneGramTable[!(lastTerm %in% twoGramTable2$lastTerm)]
    
    if(dim(oneGramTable2)[1] >= 20){
      oneprob <- oneGramTable2[1:20,.(probability=getProbabilityFrom3Gram(paste(inputtext, as.character(lastTerm)))),by=lastTerm][order(-probability)]
    }else{
      oneprob <- oneGramTable2[,.(probability=getProbabilityFrom3Gram(paste(inputtext, as.character(lastTerm)))),by=lastTerm][order(-probability)]
    }
    
    finalprob <- rbind(twoprob, oneprob)
    finalprob <- finalprob[!(lastTerm %in% stopwords()) & !(lastTerm %in% c("scsc", "ecec"))][order(-probability)]
  } else {
    oneGramTable3 <- oneGramTable[1:10]
    oneGramTable3 <- oneGramTable3[!(lastTerm %in% c("scsc", "ecec"))]
    
    oneprob <- oneGramTable3[,.(probability=getProbabilityFrom3Gram(paste(inputtext, as.character(lastTerm)))),by=lastTerm][order(-probability)]
    
    finalprob <- oneprob
  }
  
  
  
  return(finalprob[1:3])
}


###############################################################################
#test this model to one sample
make_firstTerm <- function(text){
  split_text <- str_split(text, "\\s")[[1]]
  paste_text <- paste(split_text[-length(split_text)], collapse = ' ')
  return(paste_text)
}

make_lastTerm <- function(text){
  split_text <- str_split(text, "\\s")[[1]]
  split_text <- split_text[length(split_text)]
  return(split_text)
}

make_OX <- function(answer, prediction){
  prediction_vec <- c(as.character(prediction[1,1]),
                      as.character(prediction[2,1]),
                      as.character(prediction[3,1]))
  if(answer %in% prediction_vec){
    return(TRUE)
  }else{
    return(FALSE)
  }
}



test_sample <- function(seed_number, num_of_sample){
  set.seed(seed_number)
  valid_sample <- sample_lines("./final/en_US/en_US.news.txt", n=1)
  valid_sample <- str_remove(valid_sample, pattern="\\r$")
  valid_corpus <- VCorpus(VectorSource(valid_sample))
  valid_corpus <- preprocess_corpus(valid_corpus)
  
  valid_data <- data.frame(text=sapply(valid_corpus, as.character), 
                           stringsAsFactors = FALSE)
  valid_trigram <- apply(valid_data, 1, tokenize_ngrams, n=3, n_min=3)
  valid_trigram <- unlist(valid_trigram)
  valid_trigram_table <- data.table(trigram=valid_trigram)
  valid_trigram_table <- valid_trigram_table[,.(firstTerm=make_firstTerm(trigram), 
                                                lastTerm=make_lastTerm(trigram)), by=trigram]
  
  
  eval_table <- valid_trigram_table[!(lastTerm %in% stopwords()) & !(lastTerm %in% c('scsc', 'ecec'))]
  eval_table <- eval_table[,.(answer=lastTerm, pred=as.character(predictnextword(firstTerm)[1,1]), 
                              OX=make_OX(lastTerm, predictnextword(firstTerm))), by=firstTerm]
  return(eval_table)
}



