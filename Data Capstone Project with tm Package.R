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
data1 <- readLines("./final/en_US/en_US.blogs.txt", n=10000)
data2 <- readLines("./final/en_US/en_US.news.txt", n=10000)
data3 <- readLines("./final/en_US/en_US.twitter.txt", n=10000)

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

preprocess_corpus <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, profanity)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(lemmatize_strings))
}

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


twoGramTable <- bigrams
twoGramTable <- twoGramTable %>% mutate(firstTerms=str_split_i(word, "\\s", i=1)) %>% 
  mutate(lastTerm=str_split_i(word, "\\s", i=2)) %>%
  select(frequency:lastTerm)
twoGramTable <- as.data.table(twoGramTable)


oneGramTable <- unigrams
colnames(oneGramTable) <- c('lastTerm', 'frequency')
oneGramTable <- as.data.table(oneGramTable)

#Modeling
#Is there any package dealing with katz's backoff model?
#https://github.com/ThachNgocTran/KatzBackOffModelImplementationInR/blob/master/calculateDiscount.R

#add discount rate and leftoverprob column
calcLeftOverProb = function(lastTerm, frequency, discount){
  all_freq = sum(frequency)
  
  return(1-sum((discount*frequency)/all_freq))
}


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
# Calculate the remaining probability (thanks to discounting...).
# Given that the input is grouped based on firstTerms already.


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
twoGramTable <- mutate(twoGramTable, c_est=frequency*discount)
twoGramTable <- twoGramTable[order(-c_est)]
##########################################
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
    stop(sprintf("[%s] not found in the 3-gram model.", inFirstTerms3gram))
    # The workaround could be:
    # + Write another function in which we primarily use 2-gram with support from 1-gram.
    # + Increase the corpus size so that the 3-gram can capture more diversity of words...
  }
  
  finalProb
}


##########################################################################################################
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


##########################################################################################################
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
#Make predict function

predictnextword2 <- function(inputtext){
  inputtext_processed1 <- getLastTerms(inputtext, n=2)
  inputtext_processed2 <- getLastTerms(inputtext, n=1)
  
  threeGramTable1 <- threeGramTable[firstTerms==inputtext_processed1]
  
  if(dim(threeGramTable1)[1] >= 10){
    threeprob <- threeGramTable1[1:10,.(probability=getProbabilityFrom3Gram(paste(inputtext, as.character(lastTerm)))),by=lastTerm][order(-probability)]
  }else{
    threeprob <- threeGramTable1[,.(probability=getProbabilityFrom3Gram(paste(inputtext, as.character(lastTerm)))),by=lastTerm][order(-probability)]
  }

  twoGramTable1 <- twoGramTable[firstTerms==inputtext_processed2 & !(lastTerm %in% threeGramTable1$lastTerm)]
  
  if(dim(twoGramTable1)[1] >= 10){
    twoprob <- twoGramTable1[1:10,.(probability=getProbabilityFrom3Gram(paste(inputtext, as.character(lastTerm)))),by=lastTerm][order(-probability)]
  }else{
    twoprob <- twoGramTable1[,.(probability=getProbabilityFrom3Gram(paste(inputtext, as.character(lastTerm)))),by=lastTerm][order(-probability)]
  }

  oneGramTable1 <- oneGramTable[!(lastTerm %in% twoGramTable1$lastTerm) & !(lastTerm %in% threeGramTable1$lastTerm)]
  
  if(dim(oneGramTable1)[1] >= 10){
    oneprob <- oneGramTable1[1:10,.(probability=getProbabilityFrom3Gram(paste(inputtext, as.character(lastTerm)))),by=lastTerm][order(-probability)]
  }else{
    oneprob <- oneGramTable1[,.(probability=getProbabilityFrom3Gram(paste(inputtext, as.character(lastTerm)))),by=lastTerm][order(-probability)]
  }
    
  finalprob <- rbind(threeprob, twoprob, oneprob)
  finalprob <- finalprob[order(-probability)]
  
  return(finalprob)
}

#####################################
#Now we will add MI model to our trigram model
#for this, first we make word pairs that occurs frequently
sixgramtoken <- apply(dataforngrams, 1, tokenize_ngrams, n=6, n_min=6)
sixgramdatatable <- as.data.table(unlist(sixgramtoken))
sixgramdatatable <- sixgramdatatable[!is.na(V1)]

pair_table <- sixgramdatatable[,.(V1,
                    pair1=paste(str_split_i(V1, "\\s", 1), str_split_i(V1, "\\s", 6)),
                    pair2=paste(str_split_i(V1, "\\s", 2), str_split_i(V1, "\\s", 6)),
                    pair3=paste(str_split_i(V1, "\\s", 3), str_split_i(V1, "\\s", 6)),
                    pair4=paste(str_split_i(V1, "\\s", 4), str_split_i(V1, "\\s", 6)),
                    pair5=paste(str_split_i(V1, "\\s", 5), str_split_i(V1, "\\s", 6)))]

pair_frequency <- cbind(pair_table$pair1, pair_table$pair2, pair_table$pair3, pair_table$pair4, pair_table$pair5)
pair_frequency_table <- data.table(table(pair_frequency))
colnames(pair_frequency_table) <- c('pair', 'frequency')
pair_frequency_table <- pair_frequency_table[frequency >= 3][order(-frequency)]
####################################
#add MI column
total_freq_sixgram <- dim(sixgramdatatable)[1]*5
total_freq_onegram <- sum(oneGramTable$frequency)
calculate_MI <- function(pairs, freq){
  pab <- freq/total_freq_sixgram
  pa <- oneGramTable[lastTerm==str_split_i(pairs, "\\s", 1), frequency]/total_freq_onegram
  pb <- oneGramTable[lastTerm==str_split_i(pairs, "\\s", 2), frequency]/total_freq_onegram
  MI <- log(pab/(pa*pb))
  return(MI)
}

pair_frequency_MI_table <- pair_frequency_table[, .(MI=calculate_MI(pair, frequency)), by=pair][order(-MI)]
##################################
#now combine existing model with this MI model
find_MI <- function(ao, b){
  if(paste(ao, b) %in% pair_frequency_MI_table$pair){
    MI <- pair_frequency_MI_table[pair==paste(ao, b), MI]
  }else{
    MI <- 0
  }
  return(MI)
}


#This function sum MI
sum_MI <- function(firstterm, nextword){
  len_firstterm <- length(str_split(firstterm, "\\s")[[1]])
  
  MIs <- c()
  for(m in 1:(len_firstterm-2)){
    MIs[m] <- pair_frequency_MI_table[pair==paste(str_split_i(firstterm,"\\s",m), nextword), MI][[1]]
  }
  return(sum(MIs))
}


#This function make candidate when given input text 'preprocessed' that has 3 <= length <= 9
make_candidate <- function(processed_firstterm){
  len_processed_firstterm <- length(str_split(processed_firstterm, "\\s")[[1]])
  candidate_list <- list()
  

  for(i in 1:(len_processed_firstterm-2)){
    candidate_list[[i]] <- pair_frequency_MI_table[str_split_i(pair,"\\s",1)==str_split_i(processed_firstterm,"\\s",i), .(MI, pair, nextword=str_split_i(pair,"\\s",2))][order(-MI)]
    candidate_list[[i]] <- candidate_list[[i]][1:ifelse(dim(candidate_list[[i]])[1]>=50,50,dim(candidate_list[[i]])[1])]
  }
  
  candidate_total <- candidate_list[[1]]
  if(len_processed_firstterm >3){
    for(n in 1:(len_processed_firstterm-3)){
      candidate_total <- rbind(candidate_total, candidate_list[[n+1]])
    }
  }
  
  candidate_total_MI <- candidate_total[, .(probability=getProbabilityFrom3Gram(paste(processed_firstterm, nextword))),by=nextword]
  candidate_total_MI <- candidate_total_MI[, .(MI_prob=probability*exp(sum_MI(processed_firstterm, nextword))), by=nextword]
  #########################again here
  colnames(candidate_two_MI)[1] <- 'lastTerm'
  
  # candidate_final <- rbind(candidate_one_MI, candidate_two_MI)
  # candidate_final <- unique(candidate_final[order(-MI_prob)])
  return(candidate_total_MI)
}


predict_MI_Trigram_model <- function(inputtext){
  #preprocess input
  inputcorpus <- VCorpus(VectorSource(inputtext))
  
  inputcorpus <- preprocess_corpus(inputcorpus)
  
  inputlength <- length(str_split(inputcorpus[[1]]$content, "\\s")[[1]])
  text <- inputcorpus[[1]]$content
  
  if(inputlength==2){
    return(predictnextword2(text)[1:10])
  }else if(inputlength==3){
    candidate_one <- predictnextword2(text)
    candidate_one_MI <- candidate_one[,.(probability, MI_prob=probability*exp(find_MI(str_split_i(text,"\\s",1), as.character(lastTerm,"\\s",2)))),by=lastTerm]
    
    candidate_two <- pair_frequency_MI_table[str_split_i(pair,"\\s",1)==str_split_i(text,"\\s",1), .(MI, nextword=str_split_i(pair,"\\s",2))][order(-MI)]
    candidate_two <- candidate_two[1:ifelse(dim(candidate_two)[1]>=50,50,dim(candidate_two)[1])]
    
    candidate_two_MI <- candidate_two[, .(probability=getProbabilityFrom3Gram(paste(text, nextword)), MI),by=nextword]
    candidate_two_MI <- candidate_two_MI[, .(probability, MI_prob=probability*exp(MI)),by=nextword]
    colnames(candidate_two_MI)[1] <- 'lastTerm'
    
    candidate_final <- rbind(candidate_one_MI, candidate_two_MI)
    candidate_final <- unique(candidate_final[order(-MI_prob)])
    return(candidate_final[1:10])
  }else if(inputlength==4){
    candidate_one <- predictnextword2(text)
    candidate_one_MI <- candidate_one[,.(probability, MI_prob=probability*exp(sum(find_MI(str_split_i(text,"\\s",1), as.character(lastTerm,"\\s",2)),
                                                                                  find_MI(str_split_i(text,"\\s",2), as.character(lastTerm,"\\s",2))))),by=lastTerm]
    
    candidate_two <- pair_frequency_MI_table[str_split_i(pair,"\\s",1)==str_split_i(text,"\\s",1), .(MI, nextword=str_split_i(pair,"\\s",2))][order(-MI)]
    candidate_two <- candidate_two[1:ifelse(dim(candidate_two)[1]>=50,50,dim(candidate_two)[1])]
    candidate_three <- pair_frequency_MI_table[str_split_i(pair,"\\s",1)==str_split_i(text,"\\s",2), .(MI, nextword=str_split_i(pair,"\\s",2))][order(-MI)]
    candidate_three <- candidate_three[1:ifelse(dim(candidate_three)[1]>=50,50,dim(candidate_three)[1])]
    candidate_two_three <- rbind(candidate_two, candidate_three)
    
    candidate_two_three_MI <- candidate_two_three[, .(probability=getProbabilityFrom3Gram(paste(text, nextword)), MI),by=nextword]
    candidate_two_three_MI <- candidate_two_three_MI[, .(probability, MI_prob=probability*exp(sum(find_MI(str_split_i(text,"\\s",1),as.character(nextword)),
                                                                                      find_MI(str_split_i(text,"\\s",2),as.character(nextword))))),by=nextword]
    colnames(candidate_two_three_MI)[1] <- 'lastTerm'
    
    candidate_final <- rbind(candidate_one_MI, candidate_two_three_MI)
    candidate_final <- unique(candidate_final[order(-MI_prob)])
    return(candidate_final[1:10])
  }else if(inputlength>=5){
    text <- gsub("_", " ", getLastTerms(text, n=5))
    
    candidate_one <- predictnextword2(text)
    candidate_one_MI <- candidate_one[,.(probability, MI_prob=probability*exp(sum(find_MI(str_split_i(text,"\\s",1), as.character(lastTerm,"\\s",2)),
                                                                                  find_MI(str_split_i(text,"\\s",2), as.character(lastTerm,"\\s",2)),
                                                                                  find_MI(str_split_i(text,"\\s",3), as.character(lastTerm,"\\s",2))))),by=lastTerm]
    
    candidate_two <- pair_frequency_MI_table[str_split_i(pair,"\\s",1)==str_split_i(text,"\\s",1), .(MI, nextword=str_split_i(pair,"\\s",2))][order(-MI)]
    candidate_two <- candidate_two[1:ifelse(dim(candidate_two)[1]>=50,50,dim(candidate_two)[1])]
    candidate_three <- pair_frequency_MI_table[str_split_i(pair,"\\s",1)==str_split_i(text,"\\s",2), .(MI, nextword=str_split_i(pair,"\\s",2))][order(-MI)]
    candidate_three <- candidate_three[1:ifelse(dim(candidate_three)[1]>=50,50,dim(candidate_three)[1])]
    candidate_four <- pair_frequency_MI_table[str_split_i(pair,"\\s",1)==str_split_i(text,"\\s",3), .(MI, nextword=str_split_i(pair,"\\s",2))][order(-MI)]
    candidate_four <- candidate_four[1:ifelse(dim(candidate_four)[1]>50,50,dim(candidate_four)[1])]
    candidate_two_three_four <- rbind(candidate_two, candidate_three, candidate_four)
    
    candidate_two_three_four_MI <- candidate_two_three_four[, .(probability=getProbabilityFrom3Gram(paste(text, nextword)), MI),by=nextword]
    candidate_two_three_four_MI <- candidate_two_three_four_MI[, .(probability, MI_prob=probability*exp(sum(find_MI(str_split_i(text,"\\s",1),as.character(nextword)),
                                                                                                            find_MI(str_split_i(text,"\\s",2),as.character(nextword)),
                                                                                                            find_MI(str_split_i(text,"\\s",3),as.character(nextword))))),by=nextword]
    colnames(candidate_two_three_four_MI)[1] <- 'lastTerm'
    
    candidate_final <- rbind(candidate_one_MI, candidate_two_three_four_MI)
    candidate_final <- unique(candidate_final[order(-MI_prob)])
    return(candidate_final[1:10])
  }
}


