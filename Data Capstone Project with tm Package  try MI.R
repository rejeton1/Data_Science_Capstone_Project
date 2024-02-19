#Install required packages
packages <- c('tm','readr','stringi','tokenizers','ggplot2','wordcloud',
              'SnowballC','gridExtra', 'sentimentr', 
              'textstem', 'lexicon', 'stringr', 'dplyr', 'data.table', 'LaF',
              'stopwords')

for(package in packages){
  if(!(package %in% installed.packages())){
    install.packages(package)
  }
  library(package, character.only = TRUE)
}

#Data install from blog, news, twitter
n_line <- 10000
data1 <- readLines("./final/en_US/en_US.blogs.txt", n=n_line)
data2 <- readLines("./final/en_US/en_US.news.txt", n=n_line)
data3 <- readLines("./final/en_US/en_US.twitter.txt", n=n_line)

data <- c(data1, data2, data3)

rm(data1, data2, data3)


#Make a corpus
corpus <- VCorpus(VectorSource(data))

################################################################################
#Data preprocessing
##1. to lower
#tm_map allow function that is applied to vector or textdocument also to 
# be able to applied to corpus.
##2. remove punctuations
##3. remove numbers
##4. remove profanity
##5. remove extra white space
#content_transformer allow function applied to only vector to be able to 
#be applied to textdocument.
#after when i need, i can modify the lemmatize dictionary 
#by using hash_lemmas(lexicon)


pro1 <- profanity_banned
pro2 <- profanity_arr_bad
pro3 <- profanity_racist

profanity <- union(union(pro1, pro2),pro3)

preprocess_corpus <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation, ucp=TRUE)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, profanity)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(function(x) str_remove(x, pattern="\\s$")))
  corpus <- tm_map(corpus, content_transformer(function(x) str_remove(x, pattern="^\\s")))
  return(corpus)
}

corpus <- preprocess_corpus(corpus)


#Check the corpus after preprocess
for(i in 1:50){
  print(corpus[[i]]$content)
}
################################################################################
#Preprocessing is done.

################################################################################
#Make n-grams(n=1~3)
dataforngrams <- data.frame(text=sapply(corpus, as.character), 
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
  finalprob <- finalprob[order(-probability)]
  
  return(finalprob)
}

#####################################
#Now we will add MI model to our trigram model
#for this, first we make word pairs that occurs frequently(max_distance=ten)
tableforngrams <- as.data.table(dataforngrams)

n_of_token <- function(document){
  len_document <- length(str_split(document, "\\s")[[1]])
  n <- ifelse(len_document>=10, 10, len_document)
  return(n)
}

tableforngrams <- tableforngrams[,.(token=tokenize_ngrams(text, n=n_of_token(text), 
                                                          n_min=n_of_token(text))),by=text]
tengramtable <- as.data.table(unlist(tableforngrams[,token]))
tengramtable <- tengramtable[!is.na(V1)]

pair_table <- tengramtable[,.(V1,
                    pair1=paste(str_split_i(V1, "\\s", 1), str_split_i(V1, "\\s", 10)),
                    pair2=paste(str_split_i(V1, "\\s", 2), str_split_i(V1, "\\s", 10)),
                    pair3=paste(str_split_i(V1, "\\s", 3), str_split_i(V1, "\\s", 10)),
                    pair4=paste(str_split_i(V1, "\\s", 4), str_split_i(V1, "\\s", 10)),
                    pair5=paste(str_split_i(V1, "\\s", 5), str_split_i(V1, "\\s", 10)),
                    pair6=paste(str_split_i(V1, "\\s", 6), str_split_i(V1, "\\s", 10)),
                    pair7=paste(str_split_i(V1, "\\s", 7), str_split_i(V1, "\\s", 10)),
                    pair9=paste(str_split_i(V1, "\\s", 8), str_split_i(V1, "\\s", 10)),
                    pair10=paste(str_split_i(V1, "\\s", 9), str_split_i(V1, "\\s", 10)))]

pair_frequency <- c(pair_table$pair1, pair_table$pair2, pair_table$pair3, pair_table$pair4, pair_table$pair5,
                        pair_table$pair6, pair_table$pair7, pair_table$pair8, pair_table$pair9)
rm(pair_table)
pair_frequency <- pair_frequency[!grepl("NA", pair_frequency)]
pair_frequency_table <- data.table(table(pair_frequency))[order(-N)]
colnames(pair_frequency_table) <- c('pair', 'frequency')
################################################################################
#add MI column to pairs
pair_frequency_table <- pair_frequency_table[,.(frequency, pair1=str_split_i(pair, "\\s", 1),
                                                           pair2=str_split_i(pair, "\\s", 2)),by=pair][order(-frequency)]

total_freq_tengram <- length(tengramtable$V1)
total_freq_onegram <- sum(oneGramTable$frequency)
rm(pair_frequency)

pair_frequency_table_over5 <- pair_frequency_table[frequency >= 5][order(-frequency)]

stopwords1 <- stopwords::stopwords("en", source = "nltk")
stopwords2 <- stopwords::stopwords("en", source = "snowball")
stopwords <- unique(stopwords1, stopwords2)
stopword_punct_removed <- removePunctuation(stopwords, ucp=TRUE)
stopwords <- c(stopwords, stopword_punct_removed)
stopwords <- unique(stopwords)
stopwords <- c(stopwords, 'one', 'two', 'u', 'im', 'us', 'would')

pair_frequency_table_without_stopword <- pair_frequency_table_over5[!(pair1 %in% stopwords) & !(pair2 %in% stopwords)]

#Before add MI column, filtering through AMI value.
calculate_AMI <- function(pairword1, pairword2, pairfreq){
  pab <- pairfreq/total_freq_tengram
  pa_b_ <- 1-pab
  pa_b <- sum(pair_frequency_table[(pair1!=pairword1 & pair2==pairword2), frequency])/total_freq_tengram
  pab_ <- sum(pair_frequency_table[(pair1==pairword1 & pair2!=pairword2), frequency])/total_freq_tengram
  pa <- oneGramTable[lastTerm==pairword1, frequency]/total_freq_onegram
  pa_ <- 1-pa
  pb <- oneGramTable[lastTerm==pairword2, frequency]/total_freq_onegram
  pb_ <- 1-pb
  
  AMI1 <- pab*log(pab/(pa*pb))
  AMI2 <- pab_*log(pab_/(pa*pb_))
  AMI3 <- pa_b*log(pa_b/(pa_*pb))
  AMI4 <- pa_b_*log(pa_b_/(pa_*pb_))
  
  AMI <- AMI1 + AMI2 + AMI3 + AMI4
  
  return(AMI4)
}

calculate_MI <- function(pair1, pair2, freq){
  pab <- freq/total_freq_tengram
  pa <- oneGramTable[lastTerm==pair1, frequency]/total_freq_onegram
  pb <- oneGramTable[lastTerm==pair2, frequency]/total_freq_onegram
  MI <- log(pab/(pa*pb))
  return(MI)
}

pair_frequency_AMI_table <- pair_frequency_table_without_stopword[,.(pair1, pair2, frequency, 
                                                    AMI=calculate_AMI(pair1, pair2, frequency)),by=pair][order(-AMI)]


pair_frequency_MI_table <- pair_frequency_AMI_table[, .(AMI, MI=calculate_MI(pair1,
                                                                    pair2, frequency)), by=pair][order(-MI)]
w################################################################################
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
    MIs[m] <- find_MI(str_split_i(firstterm,"\\s",m), nextword)
  }
  return(sum(MIs))
}


#This function make candidate when given input text 'preprocessed' that has 3 <= length <= 9
make_candidate <- function(processed_firstterm){
  len_processed_firstterm <- length(str_split(processed_firstterm, "\\s")[[1]])
  candidate_list <- list()
  
  candidate_one <- predictnextword(processed_firstterm)
  candidate_one_MI <- candidate_one[,.(MI_prob=probability*exp(sum_MI(processed_firstterm, lastTerm))), by=lastTerm]
  

  for(i in 1:(len_processed_firstterm-2)){
    candidate_list[[i]] <- pair_frequency_MI_table[str_split_i(pair,"\\s",1)==str_split_i(processed_firstterm,"\\s",i), .(MI, pair, nextword=str_split_i(pair,"\\s",2))][order(-MI)]
    candidate_list[[i]] <- candidate_list[[i]][1:ifelse(dim(candidate_list[[i]])[1]>=50,50,dim(candidate_list[[i]])[1])]
  }
  
  candidate_total <- candidate_list[[1]]
  if(len_processed_firstterm > 3){
    for(n in 1:(len_processed_firstterm-3)){
      candidate_total <- rbind(candidate_total, candidate_list[[n+1]])
    }
  }
  
  candidate_total_MI <- candidate_total[, .(probability=getProbabilityFrom3Gram(paste(processed_firstterm, nextword))),by=nextword]
  candidate_total_MI <- candidate_total_MI[, .(MI_prob=probability*exp(sum_MI(processed_firstterm, nextword))), by=nextword]
  colnames(candidate_total_MI)[1] <- 'lastTerm'
  
  candidate_final_MI <- rbind(candidate_one_MI, candidate_total_MI)
  candidate_final_MI <- unique(candidate_final_MI[order(-MI_prob)])
  return(candidate_final_MI)
}


predict_MI_Trigram_model <- function(inputtext){
  #preprocess input
  inputcorpus <- VCorpus(VectorSource(inputtext))
  
  inputcorpus <- preprocess_corpus(inputcorpus)
  
  inputlength <- length(str_split(inputcorpus[[1]]$content, "\\s")[[1]])
  processed_input <- inputcorpus[[1]]$content
  
  if(inputlength==2){
    candidate_final_MI <- predictnextword(processed_input)
    return(candidate_final_MI[1:5])
  }else if(inputlength>=3 & inputlength <=9){
    candidate_final_MI <- make_candidate(processed_input)
    return(candidate_final_MI[1:5])
  }else if(inputlength>=10){
    processed_input <- gsub("_", " ", getLastTerms(processed_input, n=9))
    candidate_final_MI <- make_candidate(processed_input)
    return(candidate_final_MI[1:5])
  }
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

test_sample <- function(seed_number){
  set.seed(seed_number)
  valid_sample <- sample_lines("./final/en_US/en_US.news.txt", n=1)
  valid_sample <- str_remove(valid_sample, pattern="\\r$")
  valid_corpus <- VCorpus(VectorSource(valid_sample))
  valid_corpus <- preprocess_corpus(valid_corpus)
  
  valid_data <- data.frame(text=sapply(valid_corpus, as.character), 
                           stringsAsFactors = FALSE)
  valid_table <- as.data.table(valid_data)
  
  
  valid_table <- valid_table[,.(firstTerm=make_firstTerm(text), 
                                lastTerm=make_lastTerm(text)), by=text]
  
  question <- valid_table[,firstTerm]
  answer <- valid_table[,lastTerm]
  predict_katz <- predictnextword(question)[1:5]
  predict_MI <- predict_MI_Trigram_model(question)
  
  answerdata <- data.frame(ques=question, ans=answer, pred_katz=predict_katz, pred_MI=predict_MI)
  return(answerdata)
}


#Apply this model to validation dataset(n=1000)



valid_set1 <- sample_lines("./final/en_US/en_US.blogs.txt", n=400)
valid_set2 <- sample_lines("./final/en_US/en_US.news.txt", n=300)
valid_set3 <- sample_lines("./final/en_US/en_US.twitters.txt", n=300)

valid_set <- c(valid_set1, valid_set2, valid_set3)
valid_set <- str_remove(valid_set, pattern="\\r$")

valid_corpus <- VCorpus(VectorSource(valid_set))

valid_corpus <- preprocess_corpus(valid_corpus)

valid_data <- data.frame(text=sapply(valid_corpus, as.character), 
                            stringsAsFactors = FALSE)
valid_table <- as.data.table(valid_data)


valid_table <- valid_table[,.(firstTerm=make_firstTerm(text), 
                              lastTerm=make_lastTerm(text)), by=text]

make_OX <- function(answer, prediction){
  if(answer %in% prediction){
    return(TRUE)
  }else{
    return(FALSE)
  }
}
valid_pred <- valid_table[,.(lastTerm, prediction=predict_MI_Trigram_model(firstTerm)),
                          by=fisrtTerm]
valid_pred <- valid_pred[,.(lastTerm, prediction, correct=make_OX(lastTerm, prediction)),
                         by=firstTerm]



